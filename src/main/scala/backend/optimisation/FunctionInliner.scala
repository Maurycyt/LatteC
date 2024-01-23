package backend.optimisation

import backend.generation.NameGenerator
import backend.representation.{Assignment, Call, CallVoid, Function, Instruction}

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
    val instructionCountBound: Int = 20
    val callGraph: Map[String, Set[String]] = constructCallGraph
    val isRecursive: Map[String, Boolean] = Map.from(callGraph.keys.map { name => name -> reachable(callGraph, name, name)} )
    val instrCounts: Map[String, Int] = inlinableFunctions.map { (name, fun) => name -> fun.blocks.map(_.instructions.length).sum }
    var somethingChanged = false

    for (inlinableFunctionName <- inlinableFunctions.keys) do {
      if !isRecursive(inlinableFunctionName) && instrCounts(inlinableFunctionName) < instructionCountBound then
        val inlinableFunction = inlinableFunctions(inlinableFunctionName)
        val renaming = getRenamingForFunction(inlinableFunction, function.nameGenerator)

        somethingChanged = true
    }

    somethingChanged
  }

  private def getRenamingForFunction(function: Function, nameGenerator: NameGenerator): Map[String, String] = {
    function.nonNullBlocks.flatMap { block =>
      (block.name -> nameGenerator.nextLabel) +:
        block.instructions.collect {
          case a: Assignment => a.dst.name -> nameGenerator.nextRegister
        }
    }.toMap
  }

  private def reachable(graph: Map[String, Set[String]], source: String, target: String): Boolean = {
    val q: mutable.Queue[String] = mutable.Queue.empty
    val visited: mutable.Map[String, Boolean] = mutable.Map.from(graph.keys.map { name => name -> false })
    q.addAll(graph(source))

    while q.nonEmpty do {
      val h = q.dequeue
      visited(h) = true
      q.addAll(graph(h).filterNot(visited))
    }

    visited(target)
  }

  private def constructCallGraph(using inlinableFunctions: Map[String, Function]): Map[String, Set[String]] = {
    inlinableFunctions.map { (name, function) =>
      name -> function.nonNullBlocks
        .map(_.instructions.toSet)
        .fold(Set.empty[Instruction]) { (a, b) => a ++ b }
        .collect {
          case CallVoid(name, _) => name.name
          case Call(name, _) => name.name
        }
    }
  }
}
