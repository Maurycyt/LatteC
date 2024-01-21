package backend.optimisation

import backend.generation.ExpressionAssembler
import backend.representation.{BinOp, Bitcast, ConditionalJump, Constant, Copy, Function, Inv, Jump, Neg, Phi, UnOp}
import frontend.checks.types.LatteType.TBool

import scala.collection.mutable

object Optimiser {
  def optimizeFunction(function: Function): Unit = {
    var stop = false
    given Function = function
    while !stop do {
      stop =
        !reduceConstantExpressions &&
          !removeUnreachableBlocks &&
          !collapseBlockPaths &&
          !removeCopyEquivalents
    }
  }

  private def substitute(copy: Copy)(using function: Function): Unit = {
    for (b <- function.nonNullBlocks; iIdx <- b.instructions.indices) {
      if b.instructions(iIdx) != null then
        b.instructions(iIdx) = b.instructions(iIdx).substitute(copy.dst, copy.value)
    }
  }

  private def eliminateNullInstructions(using function: Function): Unit = {
    for (b <- function.nonNullBlocks) do {
      b.instructions = b.instructions.filterNot { _ == null }
    }
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

    for (blockIdx <- function.blocks.indices.reverse) do {
      val block = function.blocks(blockIdx)
      if block != null then
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
            case Bitcast(to, from) if to.valueType.toLLVM == from.valueType.toLLVM =>
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
}
