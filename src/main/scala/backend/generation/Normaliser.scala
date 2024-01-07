package backend.generation

import backend.representation.{Assignment, Block, Copy, DefinedValue, Function, Register, ReturnVoid}
import frontend.checks.types.LatteType.TVoid

import scala.util.control.Breaks.{break, breakable}

/**
 * Converts assembled instructions into a form accepted by LLVM.
 */
object Normaliser {
	def processFunction()(using function: Function): Unit = {
		findAndRemoveCopies()
		manageEmptyLastBlock()
	}

	private def manageEmptyLastBlock()(using function: Function): Unit = {
		// If the last block is empty and the return type of the function is void, then add ReturnVoid.
		// If it is empty and the return type is non-void, then it must be unreachable, so remove it.
		val lastBlockIdx = function.numBlocks - 1
		val lastBlock = function.getBlock(lastBlockIdx)
		if lastBlock.instructions.nonEmpty then return

		function.returnType match {
			case TVoid => lastBlock += ReturnVoid
			case _ => function.removeBlock(lastBlockIdx)
		}
	}

	private def findAndRemoveCopies()(using function: Function): Unit = {
		if debug.flag then println(s"Removing copies from ${function.nameInLLVM}.")
		given visited: Array[Boolean] = Array.fill(function.numBlocks)(false)
		findAndRemoveCopiesDFS(0)
		for (i <- 0 until function.numBlocks) do {
			function.getBlock(i).instructions = function.getBlock(i).instructions.filterNot { _ == null }
		}
	}

	private def findAndRemoveCopiesDFS(blockIdx: Int)(using function: Function, visited: Array[Boolean]): Unit = {
		if visited(blockIdx) then return

		visited(blockIdx) = true
		val block = function.getBlock(blockIdx)
		if block != null then {
			for (instructionIdx <- block.instructions.indices) do {
				block.instructions(instructionIdx) match {
					case _: Copy =>
						substitute(block, instructionIdx)
					case _ => ()
				}
			}
			function.getBlockJumpsFrom(blockIdx).foreach(findAndRemoveCopiesDFS)
		}
	}

	private def substitute(block: Block, instructionIdx: Int)(using function: Function): Unit = {
		given visited: Array[Boolean] = Array.fill(function.numBlocks)(false)

		val Copy(register, newValue) = block.instructions(instructionIdx).asInstanceOf[Copy]
		block.instructions(instructionIdx) = null
		if debug.flag then println(s"Substituting $register for $newValue.")

		substituteDFS(block, register, newValue, instructionIdx + 1)
	}

	private def substituteDFS(block: Block, register: Register, newValue: DefinedValue, startFrom: Int = 0)(using function: Function, visited: Array[Boolean]): Unit = {
		if debug.flag then println(s"\tIn block ${block.name}.")
		val blockIdx = function.getBlockIndex(block)
		if visited(blockIdx) then return
		if startFrom == 0 then visited(blockIdx) = true

		breakable {
			for (i <- startFrom until block.instructions.length) do {
				val instr = block.instructions(i)
				if instr != null then {
					// If the register is overwritten, stop processing this substitution.
					instr match {
						case assignment: Assignment if assignment.dst == register => break
						case _ => ()
					}

					block.instructions(i) = instr.substitute(register, newValue)
				}
			}

			function.getBlockJumpsFrom(blockIdx).foreach { nextBlockIdx => substituteDFS(function.getBlock(nextBlockIdx), register, newValue) }
		}
	}
}
