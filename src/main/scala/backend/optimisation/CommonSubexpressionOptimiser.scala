package backend.optimisation

import backend.representation.{Assignment, Copy, Function, MemoryIndependentAssignment, MemoryUnmodifyingAssignment, MemoryUnmodifyingInstruction, Register}
import frontend.checks.types.LatteType.TVoid

import scala.collection.mutable

object CommonSubexpressionOptimiser {
  private sealed trait Domination {
    def withMemoryModification: Domination = this
    def combineWith(other: Domination): Domination = this
  }
  private object Domination {
    case object FullDomination extends Domination {
      override def withMemoryModification: Domination = MemoryModifying
      override def combineWith(other: Domination): Domination = other
    }
    case object MemoryModifying extends Domination {
      override def combineWith(other: Domination): Domination = if other == NoDomination then NoDomination else this
    }
    case object NoDomination extends Domination
  }
  import Domination.*
  
  def optimiseFunction(using function: Function): Boolean = {
    var somethingChanged = false
    var stop = false
    while !stop do {
      stop = !eliminateCommonSubexpressions
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

  private type DominationTable = Array[Array[Domination]]

  private def eliminateCommonSubexpressions(using function: Function): Boolean = {
    var somethingChanged = false
    val (prefixDominationInBlocks, suffixDominationInBlocks, dominationBetweenBlocks) = constructDominationTables
    val commonSubexpressions = collectCommonSubexpressions

    val removedInstructions: mutable.Set[(Int, Int)] = mutable.Set.empty
    for (similarInstructions <- commonSubexpressions;
         firstInstructionIdxs <- similarInstructions;
         secondInstructionIdxs <- similarInstructions) do {
      if secondInstructionIdxs != firstInstructionIdxs && !removedInstructions.contains(firstInstructionIdxs) && !removedInstructions.contains(secondInstructionIdxs) then {
        val domination = checkInstructionDomination(firstInstructionIdxs, secondInstructionIdxs, prefixDominationInBlocks, suffixDominationInBlocks, dominationBetweenBlocks)
        val firstInstruction: Assignment = function.blocks(firstInstructionIdxs._1).instructions(firstInstructionIdxs._2).asInstanceOf[Assignment]
        val secondInstruction: Assignment = function.blocks(secondInstructionIdxs._1).instructions(secondInstructionIdxs._2).asInstanceOf[Assignment]
        (domination, secondInstruction) match {
          // We can only substitute assignments which do not modify memory.
          case (FullDomination, _: MemoryUnmodifyingAssignment) =>
            function.blocks(secondInstructionIdxs._1).instructions(secondInstructionIdxs._2) = null
            removedInstructions.add(secondInstructionIdxs)
            somethingChanged = true
            substitute(Copy(secondInstruction.dst, firstInstruction.dst))
          case (MemoryModifying, _: MemoryIndependentAssignment) =>
            somethingChanged = true
            function.blocks(secondInstructionIdxs._1).instructions(secondInstructionIdxs._2) = null
            removedInstructions.add(secondInstructionIdxs)
            substitute(Copy(secondInstruction.dst, firstInstruction.dst))
          case _ =>
        }
      }
    }

    eliminateNullInstructions

    somethingChanged
  }

  private def constructDominationTables(using function: Function): (DominationTable, DominationTable, DominationTable) = {
    // In a block, how a certain prefix or suffix of instructions is dominated by the first instruction in the prefix or suffix.
    val prefixDominationInBlocks = Array.fill(function.blocks.length)(Array.empty[Domination])
    val suffixDominationInBlocks = Array.fill(function.blocks.length)(Array.empty[Domination])
    // Between two blocks, how the first block dominated the second block.
    val dominationBetweenBlocks = Array.fill(function.blocks.length)(Array.fill[Domination](function.blocks.length)(NoDomination))
    // How domination changes from the beginning to the end of the block.
    val dominationAcrossBlock = Array.fill[Domination](function.blocks.length)(null)

    for (blockIdx <- function.blocks.indices) do {
      val block = function.blocks(blockIdx)
      if block != null then {
        val liIdx = block.instructions.length - 1 // last instruction index
        prefixDominationInBlocks(blockIdx) = Array.fill(block.instructions.length)(NoDomination)
        suffixDominationInBlocks(blockIdx) = Array.fill(block.instructions.length)(NoDomination)
        prefixDominationInBlocks(blockIdx)(0) = FullDomination
        suffixDominationInBlocks(blockIdx)(liIdx) = FullDomination

        for (i <- 1 to liIdx) do {
          val pInstr = block.instructions(i)
          prefixDominationInBlocks(blockIdx)(i) = pInstr match {
            case u: MemoryUnmodifyingInstruction => prefixDominationInBlocks(blockIdx)(i - 1)
            case _ => prefixDominationInBlocks(blockIdx)(i - 1).withMemoryModification
          }
          val sInstr = block.instructions(liIdx - i)
          suffixDominationInBlocks(blockIdx)(liIdx - i) = sInstr match {
            case u: MemoryUnmodifyingInstruction => suffixDominationInBlocks(blockIdx)(liIdx - i + 1)
            case _ => suffixDominationInBlocks(blockIdx)(liIdx - i + 1).withMemoryModification
          }
        }

        dominationAcrossBlock(blockIdx) = prefixDominationInBlocks(blockIdx)(liIdx)
      }
    }
    
    val nnIndices = function.nonNullBlockIndices

    for (iteration <- nnIndices;
         blockBelow <- nnIndices;
         blockAbove <- nnIndices
    ) do {
      dominationBetweenBlocks(blockAbove)(blockBelow) =
        if function.getBlockJumpsTo(blockBelow).isEmpty then
          NoDomination
        else
          function.getBlockJumpsTo(blockBelow).foldLeft(FullDomination: Domination) { (domination, precedingBlock) =>
            domination.combineWith(
              if precedingBlock == blockAbove then
                FullDomination
              else
                dominationAcrossBlock(precedingBlock).combineWith(dominationBetweenBlocks(blockAbove)(precedingBlock))
            )
          }
    }

    (prefixDominationInBlocks, suffixDominationInBlocks, dominationBetweenBlocks)
  }

  private def collectCommonSubexpressions(using function: Function): Seq[Seq[(Int, Int)]] = {
    val defaultRegister = Register(TVoid, "")
    function.nonNullBlockIndices.foldLeft(Map.empty[Assignment, Set[(Int, Int)]]) { (mb, bIdx) =>
      val block = function.blocks(bIdx)
      block.instructions.indices.foldLeft(mb) { (m, iIdx) =>
        val instr = block.instructions(iIdx)
        instr match {
          case a: Assignment => m.updatedWith(a.replaceDst(defaultRegister)) { v => Some(v.getOrElse(Set.empty) + ((bIdx, iIdx))) }
          case _ => m
        }
      }
    }.collect { case (_, vs) if vs.size > 1 => vs.toSeq }.toSeq
  }

  private def checkInstructionDomination(
    firstInstruction: (Int, Int), // block, instruction indices
    secondInstruction: (Int, Int),
    prefixDominationInBlocks: DominationTable,
    suffixDominationInBlocks: DominationTable,
    dominationBetweenBlocks: DominationTable
  )(using function: Function): Domination = {
    val (fb, fi) = firstInstruction
    val (sb, si) = secondInstruction
    if fb == sb then {
      if fi >= si then
        NoDomination
      else
        // fi < si
        var domination: Domination = FullDomination
        for (i <- fi + 1 until si) do {
          function.blocks(fb).instructions(i) match {
            case _: MemoryUnmodifyingInstruction =>
            case _ => domination = domination.withMemoryModification
          }
        }
        domination
    } else {
      suffixDominationInBlocks(fb)(fi)
        .combineWith(dominationBetweenBlocks(fb)(sb))
        .combineWith(prefixDominationInBlocks(sb)(si))
    }
  }
}
