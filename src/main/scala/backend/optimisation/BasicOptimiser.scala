package backend.optimisation

import backend.generation.ExpressionAssembler
import backend.representation.{BinOp, Bitcast, Block, CallVoid, ConditionalJump, Constant, Copy, Eq, Function, Inv, Jump, MemoryUnmodifyingAssignment, Neg, Phi, Register, UnOp}
import frontend.checks.types.LatteType.{TBool, TInt}

import scala.collection.mutable

object BasicOptimiser {
  def optimiseFunction(using function: Function): Boolean = {
    var somethingChanged = false
    var stop = false
    while !stop do {
      stop  = !reduceConstantExpressions
      stop &= !collapseBlockPaths
      stop &= !removeCopyEquivalents
      stop &= !removeUnreachableBlocks
      stop &= !removeUnusedRegisters
      stop &= !removeUnnecessaryGarbageCollection
      if !stop then somethingChanged = true
    }
    somethingChanged
  }

  private def substitute(copy: Copy)(using function: Function): Unit = {
    for (b <- function.nonNullBlocks; iIdx <- b.instructions.indices) {
      if b.instructions(iIdx) != null then
        b.instructions(iIdx) = b.instructions(iIdx).substitute(copy.dst, copy.value)
    }
  }

  private def eliminateNullInstructions(using function: Function): Unit = {
    function.nonNullBlocks.foreach(_.removeNulls())
  }

  private def reduceConstantExpressions(using function: Function): Boolean = {
    var somethingChanged = false
    var stop = false

    while !stop do {
      var nothingChangedThisLoop = true

      for (b <- function.nonNullBlocks; iIdx <- b.instructions.indices) do {
        val instruction = b.instructions(iIdx)

        if instruction != null then {
          val newInstruction = instruction match {
            case UnOp(dst, op, Constant(t, v)) => op match {
              case Inv => Copy(dst, Constant(t, -v))
              case Neg => Copy(dst, Constant(t, 1 - v))
            }
            case BinOp(dst, Constant(lt, lv), op, Constant(rt, rv)) =>
              Copy(dst, ExpressionAssembler.operateOnTwoIntegers(lv, op, rv))
            case BinOp(dst, r1, Eq, r2) if r1 == r2 =>
              Copy(dst, Constant(TBool, 1))
            case cj @ ConditionalJump(Constant(TBool, v), ifTrue, ifFalse) =>
              if v == 1 then
                function.removeJump(b.name, ifFalse)
                Jump(ifTrue)
              else
                function.removeJump(b.name, ifTrue)
                Jump(ifFalse)
            case _ => instruction
          }

          newInstruction match {
            case copy: Copy =>
              nothingChangedThisLoop = false
              b.instructions(iIdx) = null
              substitute(copy)
            case jump: Jump if instruction != jump =>
              nothingChangedThisLoop = false
              b.instructions(iIdx) = jump
            case _ =>
          }
        }
      }

      if nothingChangedThisLoop then
        stop = true
      else
        somethingChanged = true
    }

    if somethingChanged then eliminateNullInstructions

    somethingChanged
  }

  private def removeUnreachableBlocks(using function: Function): Boolean = {
    val visited: Array[Boolean] = Array.fill(function.numBlocks)(false)
    val q: mutable.Queue[Int] = mutable.Queue(0)

    while q.nonEmpty do {
      val h = q.dequeue
      visited(h) = true
      q.appendAll(function.getBlockJumpsFrom(h).filterNot(visited))
    }

    var somethingChanged = false

    visited.indices.foreach { blockIdx =>
      if !visited(blockIdx) && function.blocks(blockIdx) != null then
        somethingChanged = true
        function.removeBlock(blockIdx)
    }

    somethingChanged
  }

  private def collapseBlockPaths(using function: Function): Boolean = {
    var somethingChanged = false

    for (blockIdx <- function.nonNullBlockIndices.reverse) do {
      val block = function.blocks(blockIdx)
      if block != null then // We check not null because we remove blocks when collapsing jumps.
        val lastInstruction = block.instructions.last
        lastInstruction match {
          case Jump(nextBlock) if function.getBlockJumpsTo(nextBlock).length == 1 && nextBlock != block.name =>
            somethingChanged = true
            function.collapseJump(block.name, nextBlock)
          case _ =>
        }
    }

    somethingChanged
  }

  private def removeCopyEquivalents(using function: Function): Boolean = {
    var somethingChanged = false
    var stop = false

    while !stop do {
      var nothingChangedThisLoop = true
      for (b <- function.nonNullBlocks; iIdx <- b.instructions.indices) do {
        if b.instructions(iIdx) != null then {
          b.instructions(iIdx) match {
            case c: Copy =>
              nothingChangedThisLoop = false
              b.instructions(iIdx) = null
              substitute(c)
            case p: Phi if p.cases.map(_.value).filter(_ != p.dst).toSet.size == 1 =>
              nothingChangedThisLoop = false
              b.instructions(iIdx) = null
              substitute(Copy(p.dst, p.cases.map(_.value).filter(_ != p.dst).head))
            case Bitcast(to, from, targetType) if from.valueType.toLLVM == targetType.toLLVM =>
              nothingChangedThisLoop = false
              b.instructions(iIdx) = null
              substitute(Copy(to, from))
            case _ => ()
          }
        }
      }
      if nothingChangedThisLoop then
        stop = true
      else
        somethingChanged = true
    }

    if somethingChanged then eliminateNullInstructions

    somethingChanged
  }

  private def removeUnusedRegisters(using function: Function): Boolean = {
    var somethingChanged = false

    for (block <- function.nonNullBlocks; iIdx <- block.instructions.indices) do {
      val instr = block.instructions(iIdx)
      instr match {
        case a: MemoryUnmodifyingAssignment =>
          val dst = a.dst
          var isDstUsed = false
          for (block2 <- function.nonNullBlocks; instr2 <- block2.instructions) do {
            if instr2 != null && instr2.substitute(dst, Register(TInt, "illegalName")) != instr2 then isDstUsed = true
          }
          if !isDstUsed then
            block.instructions(iIdx) = null
            somethingChanged = true

        case _ =>
      }
    }

    if somethingChanged then eliminateNullInstructions

    somethingChanged
  }

  private def removeUnnecessaryGarbageCollection(using function: Function): Boolean = {
    var somethingChanged = false
    // Three-valued DFS.
    val visited: Array[Int] = Array.fill(function.blocks.length)(0)

    // Removes unnecessary garbage collection in a block
    // and returns whether there may be uncleared decreased references.
    def removeInBlock(bIdx: Int): Boolean = {
      if visited(bIdx) == 1 then return true
      visited(bIdx) = 1
      var mayHaveUnclearedDecreasedReferences: Boolean = function.getBlockJumpsTo(bIdx).map(removeInBlock).fold(false)(_ || _)
      visited(bIdx) = 2

      val block: Block = function.blocks(bIdx)

      for (iIdx <- block.instructions.indices) do {
        val instr = block.instructions(iIdx)
        if instr != null then instr match {
          case c: CallVoid =>
            if c.name.name == "@clearUnboundPointers" then
              if mayHaveUnclearedDecreasedReferences then
                mayHaveUnclearedDecreasedReferences = false
              else
                block.instructions(iIdx) = null
                somethingChanged = true
            else if Seq("@decreaseRefCount", "@registerString", "@registerArray", "@registerObject").contains(c.name.name) then
              mayHaveUnclearedDecreasedReferences = true
          case _ =>
        }
      }

      mayHaveUnclearedDecreasedReferences
    }

    function.nonNullBlockIndices.foreach(removeInBlock)

    if somethingChanged then eliminateNullInstructions

    somethingChanged
  }
}
