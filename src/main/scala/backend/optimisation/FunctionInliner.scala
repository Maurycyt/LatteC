package backend.optimisation

import backend.representation.{Block, Call, CallVoid, ConditionalJump, Copy, DefinedValue, Function, Instruction, Jump, Phi, PhiCase, Register, Return, ReturnVoid, SomeCall}
import frontend.checks.types.LatteType.TVoid

import scala.collection.mutable

object FunctionInliner {
  def inlineFunction(using function: Function, inlinableFunctions: Map[String, Function]): Boolean = {
    var somethingChanged = false
    var stop = false
    while !stop do {
      stop = !doInlineFunction
      if !stop then somethingChanged = true
    }
    somethingChanged
  }

  private def doInlineFunction(using function: Function, inlinableFunctions: Map[String, Function]): Boolean = {
    val inlinerInstructionBound: Int = 200
    val inlinerSize: Int = function.nonNullBlocks.map(_.instructions.length).sum

    val inlinedInstructionBound: Int = 20
    val callGraph: Map[String, Set[String]] = constructCallGraph
    val isRecursive: Map[String, Boolean] = Map.from(callGraph.keys.map { name => name -> reachable(callGraph, name, name)} )
    val instrCounts: Map[String, Int] = inlinableFunctions.map { (name, fun) => name -> fun.nonNullBlocks.map(_.instructions.length).sum }
    var somethingChanged = false

    for (inlinableFunctionName <- inlinableFunctions.keys if !somethingChanged) do {
      if !isRecursive(inlinableFunctionName)
        && instrCounts(inlinableFunctionName) < inlinedInstructionBound
        && (inlinerSize < inlinerInstructionBound || instrCounts(inlinableFunctionName) == 1)
      then
        val inlinableFunction = inlinableFunctions(inlinableFunctionName)

        for (bIdx <- function.nonNullBlockIndices if !somethingChanged;
             iIdx <- function.blocks(bIdx).instructions.indices if !somethingChanged
        ) do {
          val instr = function.blocks(bIdx).instructions(iIdx)
          instr match {
            case c: SomeCall if c.name.name == inlinableFunctionName =>
              // Prepare data for injection.
              val callingBlock = function.blocks(bIdx)
              val returnBlock = function.addBlock(Some(callingBlock.name + "r"))
              val returnPhiCases: mutable.ArrayBuffer[PhiCase] = mutable.ArrayBuffer.empty
              given renaming: Map[String, String] = inlinableFunction.getRenaming(function.nameGenerator)

              returnBlock.instructions = callingBlock.instructions.takeRight(callingBlock.instructions.length - iIdx - 1)
              val inlinableArgSources = inlinableFunction.arguments.map(inlinableFunction.argumentsInfo).map(_.source.rename).filter(_.valueType != TVoid).map(_.asInstanceOf[Register])

              // Inject the blocks.
              val argsWithSources = c.args.filter(_.valueType != TVoid).map(_.asInstanceOf[DefinedValue]).zip(inlinableArgSources)
              callingBlock.instructions.takeInPlace(iIdx).appendAll(
                argsWithSources.map { (arg, sourceInFunction) => Copy(sourceInFunction, arg) }
                  :+ Jump(renaming("entry"))
              )

              inlinableFunction.nonNullBlocks.map(_.copy(using renaming)).foreach { newBlock =>
                val instrs = newBlock.instructions
                instrs.last match {
                  case ReturnVoid => instrs(instrs.length - 1) = Jump(returnBlock.name)
                  case Return(v) =>
                    instrs(instrs.length - 1) = Jump(returnBlock.name)
                    returnPhiCases += PhiCase(newBlock.name, v)
                  case _ =>
                }
                function.addBlockIgnoreJumps(newBlock)
              }

              // Gather return value.
              c match {
                case cd: Call => returnBlock.instructions.prepend(Phi(cd.dst, returnPhiCases.toSeq: _*))
                case _: CallVoid =>
              }

              def renamePhiSources(block: Block, from: String, to: String): Unit = {
                for (iIdx <- block.instructions.indices) do {
                  block.instructions(iIdx) = block.instructions(iIdx) match {
                    case phi: Phi =>
                      Phi(phi.dst, phi.cases.map { phiCase =>
                        PhiCase(if phiCase.blockName == from then to else phiCase.blockName, phiCase.value)
                      }: _*)
                    case other => other
                  }
                }
              }

              // Rebind Phis which take from split block.
              returnBlock.instructions.last match {
                case Jump(name) =>
                  renamePhiSources(function.getBlock(name), callingBlock.name, returnBlock.name)
                case ConditionalJump(_, name1, name2) =>
                  renamePhiSources(function.getBlock(name1), callingBlock.name, returnBlock.name)
                  renamePhiSources(function.getBlock(name2), callingBlock.name, returnBlock.name)
                case _ =>
              }
              
              // Clean up.
              function.rewireAndRename()

              somethingChanged = true
            case _ =>
          }
        }
    }

    somethingChanged
  }

  private def reachable(graph: Map[String, Set[String]], source: String, target: String): Boolean = {
    val q: mutable.Queue[String] = mutable.Queue.empty
    val visited: mutable.Map[String, Boolean] = mutable.Map.empty.withDefaultValue(false)
    q.addAll(graph(source))

    while q.nonEmpty do {
      val h = q.dequeue
      visited(h) = true
      q.addAll(graph.getOrElse(h, Set.empty)
        .filterNot(visited))
    }

    visited(target)
  }

  private def constructCallGraph(using inlinableFunctions: Map[String, Function]): Map[String, Set[String]] = {
    inlinableFunctions.map { (inlinableName, inlinableFunction) =>
      val instructions = inlinableFunction.nonNullBlocks
        .map(_.instructions)
        .fold(Seq.empty[Instruction]) { (a, b) => a ++ b }
      val calledNames = instructions.collect {
        case cv: CallVoid => cv.name.name
        case c: Call => c.name.name
      }.toSet
      inlinableName -> calledNames
    }
  }
}
