package backend.generation

import backend.representation.{Assignment, Block, ConditionalJump, Copy, Function, Jump, Phi, PhiCase, Register, Return, ReturnVoid}
import frontend.checks.types.LatteType.TVoid

import scala.collection.mutable

/**
 * Converts assembled instructions into a form accepted by LLVM.
 */
class Normaliser()(using function: Function) {
	def processFunction(): Unit = {
		transformIntoSSA()
		findAndRemoveCopies()
		manageUnterminatedBlocks()
//		removeUnreachableBlocks()
	}

	private def transformIntoSSA(): Unit = {
		val argumentRegisters: Set[Register] = function.argumentsInfo.collect { case (_, SymbolSourceInfo(_, _, r: Register)) => r }.toSet
		val bodyRegisters: Set[Register] = function.blocks
			.flatMap { block => block.instructions.collect { case assignment: Assignment => assignment.dst } }
			.groupBy(identity)
			.map { (register, copies) => register -> copies.size }
			.filter(_._2 > 1)
			.keySet
		val registersToTransform: Set[Register] = argumentRegisters ++ bodyRegisters

		if debug.flag then println(s"Registers to transform in $function: $registersToTransform.")

		registersToTransform.foreach { register => transformRegisterIntoSSA(register, argumentRegisters) }
	}

	private def transformRegisterIntoSSA(register: Register, argumentRegisters: Set[Register]): Unit = {
		val occurrencesInBlocks: Seq[Int] = function.blocks.indices.map(renameRegisterInBlock(register))
		val phiCasesForBlocks: Array[Option[mutable.Map[String, Register]]] =
			function.blocks.indices.map(collectPhiCasesForBlock(register, argumentRegisters, occurrencesInBlocks)).toArray
		println(s"phiCasesForBlocks: ${phiCasesForBlocks.mkString("Array(", ", ", ")")}")
		reducePhiCases(occurrencesInBlocks, phiCasesForBlocks)

		function.blocks.indices.foreach { blockIdx =>
			phiCasesForBlocks(blockIdx).foreach { phiCasesForBlock =>
				function.blocks(blockIdx).instructions.prepend(
					Phi(
						Register(register.valueType, s"$register.$blockIdx.0"),
						phiCasesForBlock.map { (n, r) => PhiCase(n, r) }.toSeq: _*
					)
				)
			}
		}
	}

	private def renameRegisterInBlock(register: Register)(blockIdx: Int): Int = {
		var occurrences = 0
		val block = function.blocks(blockIdx)
		substituteRegisterInBlock(register, Register(register.valueType, s"$register.$blockIdx.0"), block, 0)
		for (instrIdx <- block.instructions.indices) do {
			block.instructions(instrIdx) match {
				case assignment: Assignment if assignment.dst == register =>
					occurrences += 1
					val oldRegister = Register(register.valueType, s"$register.$blockIdx.${occurrences-1}")
					val newRegister = Register(register.valueType, s"$register.$blockIdx.$occurrences")
					block.instructions(instrIdx) = assignment.replaceDst(newRegister)
					substituteRegisterInBlock(oldRegister, newRegister, block, instrIdx + 1)
				case _ => ()
			}
		}
		occurrences
	}

	private def collectPhiCasesForBlock
	(register: Register, argumentRegisters: Set[Register], occurrencesInBlocks: Seq[Int])
	(blockIdx: Int): Some[mutable.Map[String, Register]] = {
		Some(blockIdx match {
			case 0 => if argumentRegisters.contains(register) then mutable.Map("entry" -> register) else mutable.Map.empty
			case nonEntryIdx => mutable.Map.from(function.getBlockJumpsTo(nonEntryIdx).map { sourceBlockIdx =>
				function.getBlockName(sourceBlockIdx) -> Register(register.valueType, s"$register.$sourceBlockIdx.${occurrencesInBlocks(sourceBlockIdx)}")
			})
		})
	}

	private def reducePhiCases(occurrencesInBlocks: Seq[Int], phiCasesForBlocks: Array[Option[mutable.Map[String, Register]]]): Unit = {
		var stop = false
		while !stop do {
			var nothingChanged = true

			for (thisBlockIdx <- phiCasesForBlocks.indices) do {
				phiCasesForBlocks(thisBlockIdx) match {
					case None => ()
					case Some(cases) =>
						if cases.isEmpty then
							nothingChanged = false
							phiCasesForBlocks(thisBlockIdx) = None
							if occurrencesInBlocks(thisBlockIdx) == 0 then
								function.getBlockJumpsFrom(thisBlockIdx).foreach { targetBlockIdx =>
									phiCasesForBlocks(targetBlockIdx).foreach(_.remove(function.getBlockName(thisBlockIdx)))
								}
				}
			}

			if nothingChanged then stop = true
		}
	}

	private def substituteRegisterInBlock(register: Register, newRegister: Register, block: Block, fromInstruction: Int): Unit = {
		for (i <- fromInstruction until block.instructions.length) do {
			block.instructions(i) = block.instructions(i).substitute(register, newRegister)
		}
	}

	private def findAndRemoveCopies(): Unit = {
		if debug.flag then println(s"Removing copies from ${function.nameInLLVM}.")

		var stop = false
		while !stop do {
			var nothingChanged = true
			for (b <- function.blocks; iIdx <- b.instructions.indices) do {
				if b.instructions(iIdx) != null then
					b.instructions(iIdx) match {
						case c: Copy =>
							nothingChanged = false
							b.instructions(iIdx) = null
							substitute(c)
						case p: Phi if p.cases.map(_.value).toSet.size == 1 =>
							nothingChanged = false
							b.instructions(iIdx) = null
							substitute(Copy(p.dst, p.cases.head.value))
						case _ => ()
					}
			}
			if nothingChanged then stop = true
		}

		for (bIdx <- function.blocks.indices) do {
			function.getBlock(bIdx).instructions = function.getBlock(bIdx).instructions.filterNot { _ == null }
		}
	}

	private def substitute(copy: Copy): Unit = {
		if debug.flag then println(s"\tFound $copy.")
		for (b <- function.blocks; iIdx <- b.instructions.indices) {
			if b.instructions(iIdx) != null then
				b.instructions(iIdx) = b.instructions(iIdx).substitute(copy.dst, copy.value)
		}
	}

	private def manageUnterminatedBlocks(): Unit = {
		// If a block is empty, lacks a jump or a return instruction,
		// and the return type of the function is void, then add ReturnVoid.
		// Otherwise, it must be unreachable, so make it loop on itself.
		// We leave removal of unreachable blocks to optimisation.

		for (block <- function.blocks) do {
			if block.instructions.isEmpty || !block.instructions.last.isInstanceOf[Jump | ConditionalJump | ReturnVoid.type | Return] then
				function.returnType match {
					case TVoid => block += ReturnVoid
					case _ => block += Jump(block.name)
				}
		}
	}

	private def removeUnreachableBlocks(): Unit = {
		var stop = false
		while !stop do {
			var nothingChanged = true
			for (blockIdx <- 1 until function.blocks.length) {
				if function.blocks(blockIdx) != null && function.getBlockJumpsTo(blockIdx).isEmpty then
					function.removeBlock(blockIdx)
					nothingChanged = false
			}
			if nothingChanged then stop = true
		}
	}
}
