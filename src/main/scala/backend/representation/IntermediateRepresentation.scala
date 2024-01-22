package backend.representation

import backend.generation.*
import frontend.checks.types.CompilerType
import frontend.checks.types.LatteType.{TBool, TInt, TVoid}

import scala.annotation.targetName
import scala.collection.mutable

// Unary Operators

sealed trait UnaryOperator

case object Inv extends UnaryOperator
case object Neg extends UnaryOperator

object UnaryOperator {
	private val unOpMapping: Map[String, UnaryOperator] = Map(
		"-" -> Inv, "!" -> Neg
	)

	def from: String => UnaryOperator = unOpMapping.apply
}

// Binary Operators

sealed trait BinaryOperator

case object Plus extends BinaryOperator
case object Minus extends BinaryOperator
case object Mul extends BinaryOperator
case object Div extends BinaryOperator
case object Mod extends BinaryOperator
case object Eq extends BinaryOperator
case object Ne extends BinaryOperator
case object Lt extends BinaryOperator
case object Le extends BinaryOperator
case object Gt extends BinaryOperator
case object Ge extends BinaryOperator
case object And extends BinaryOperator
case object Or extends BinaryOperator

object BinaryOperator {
	private val binOpMapping: Map[String, BinaryOperator] = Map(
		"+" -> Plus, "-" -> Minus, "*" -> Mul, "/" -> Div, "%" -> Mod,
		"==" -> Eq, "!=" -> Ne, "<" -> Lt, "<=" -> Le, ">" -> Gt, ">=" -> Ge,
		"&&" -> And, "||" -> Or
	)

	def from: String => BinaryOperator = binOpMapping.apply
}

// Constants, Registers, and Labels

sealed trait Source {
	def valueType: CompilerType
	def toStringWithoutType: String
	def toStringWithType: String = s"${valueType.toLLVM} $toStringWithoutType"
	override def toString: String = toStringWithoutType
}

sealed trait Value extends Source
sealed trait DefinedValue extends Value
sealed trait Name extends Source { def name: String; override def toStringWithoutType: String = name }

case class Constant(valueType: CompilerType, value: Long) extends DefinedValue {
	override def toStringWithoutType: String = valueType match {
		case TInt | TBool => value.toString
		case TVoid => "voidUnit"
		case _ => if value == 0 then "null" else value.toString
	}
}
case class Register(valueType: CompilerType, name: String) extends DefinedValue with Name
case class Label(valueType: CompilerType, name: String) extends Name

// Instructions

sealed trait Instruction {
	def substitute(reg: Register, newValue: DefinedValue): Instruction = this
}

// An assignment is an instruction which sets the value of a register.
sealed trait Assignment extends Instruction {
	def dst: Register
	def replaceDst(newDst: Register): Assignment
}

// An instruction which cannot influence memory.
sealed trait MemoryUnmodifyingInstruction extends Instruction
// An assignment which cannot influence memory, but may read from it.
sealed trait MemoryUnmodifyingAssignment extends Assignment with MemoryUnmodifyingInstruction
// An assignment which can neither influence memory, nor read from it.
sealed trait MemoryIndependentAssignment extends MemoryUnmodifyingAssignment 


case class Copy(dst: Register, value: DefinedValue) extends MemoryIndependentAssignment {
	override def substitute(reg: Register, newValue: DefinedValue): Copy = if reg == value then copy(value = newValue) else this
	override def replaceDst(newDst: Register): Copy = copy(dst = newDst)
}

case class PhiCase(blockName: String, value: DefinedValue) {
	def substitute(reg: Register, newValue: DefinedValue): PhiCase = if reg == value then copy(value = newValue) else this
}

case class Phi(dst: Register, cases: PhiCase*) extends MemoryIndependentAssignment {
	override def substitute(reg: Register, newValue: DefinedValue): Phi = Phi(dst, cases.map(_.substitute(reg, newValue)): _*)
	override def replaceDst(newDst: Register): Phi = Phi(newDst, cases: _*)
}

case class BitcastStringConstant(dst: Register, stringConstant: String) extends MemoryIndependentAssignment {
	override def replaceDst(newDst: Register): BitcastStringConstant = copy(dst = newDst)
}

case class Bitcast(dst: Register, arg: DefinedValue, targetType: CompilerType) extends MemoryIndependentAssignment {
	override def substitute(reg: Register, newValue: DefinedValue): Bitcast = if reg == arg then copy(arg = newValue) else this
	override def replaceDst(newDst: Register): Bitcast = copy(dst = newDst)
}

case class UnOp(dst: Register, op: UnaryOperator, arg: DefinedValue) extends MemoryIndependentAssignment {
	override def substitute(reg: Register, newValue: DefinedValue): UnOp = if reg == arg then copy(arg = newValue) else this
	override def replaceDst(newDst: Register): UnOp = copy(dst = newDst)
}

case class BinOp(dst: Register, arg1: DefinedValue, op: BinaryOperator, arg2: DefinedValue) extends MemoryIndependentAssignment {
	override def substitute(reg: Register, newValue: DefinedValue): BinOp = copy(
		arg1 = if arg1 == reg then newValue else arg1,
		arg2 = if arg2 == reg then newValue else arg2
	)
	override def replaceDst(newDst: Register): BinOp = copy(dst = newDst)
}

case class GetElementPtr(dst: Register, ptr: DefinedValue, idx: DefinedValue, idxs: DefinedValue*) extends MemoryIndependentAssignment {
	override def substitute(reg: Register, newValue: DefinedValue): GetElementPtr =
		GetElementPtr(
			dst,
			if ptr == reg then newValue.asInstanceOf[Register] else ptr,
			if idx == reg then newValue else idx,
			idxs.map { idx => if idx == reg then newValue else idx }: _*
		)
	override def replaceDst(newDst: Register): GetElementPtr = GetElementPtr(newDst, ptr, idx, idxs: _*)
}

case class PtrToInt(dst: Register, ptr: Register) extends MemoryIndependentAssignment {
	override def substitute(reg: Register, newValue: DefinedValue): PtrToInt = if ptr == reg then PtrToInt(dst, newValue.asInstanceOf[Register]) else this
	override def replaceDst(newDst: Register): PtrToInt = PtrToInt(newDst, ptr)
}

case class Jump(blockName: String) extends MemoryUnmodifyingInstruction

case class ConditionalJump(arg: DefinedValue, blockNameTrue: String, blockNameFalse: String) extends MemoryUnmodifyingInstruction {
	override def substitute(reg: Register, newValue: DefinedValue): ConditionalJump = if arg == reg then copy(arg = newValue) else this
}

case class PtrStore(ptr: Register, arg: DefinedValue) extends Instruction {
	override def substitute(reg: Register, newValue: DefinedValue): PtrStore = copy(
		if ptr == reg then newValue.asInstanceOf[Register] else ptr,
		if arg == reg then newValue else arg
	)
}

case class PtrLoad(dst: Register, ptr: Register) extends MemoryUnmodifyingAssignment {
	override def substitute(reg: Register, newValue: DefinedValue): PtrLoad = if ptr == reg then copy(ptr = newValue.asInstanceOf[Register]) else this
	override def replaceDst(newDst: Register): PtrLoad = copy(dst = newDst)
}

case object ReturnVoid extends MemoryUnmodifyingInstruction

case class Return(arg: DefinedValue) extends MemoryUnmodifyingInstruction {
	override def substitute(reg: Register, newValue: DefinedValue): Return = if arg == reg then Return(newValue) else this
}

case class CallVoid(name: Name, args: Value*) extends Instruction {
	override def substitute(reg: Register, newValue: DefinedValue): CallVoid = CallVoid(
		name,
		args.map { arg => if arg == reg then newValue else arg }: _*
	)
}

case class Call(dst: Register, name: Name, args: Value*) extends Assignment {
	override def substitute(reg: Register, newValue: DefinedValue): Call = Call(
		dst,
		name,
		args.map { arg => if arg == reg then newValue else arg }: _*
	)
	override def replaceDst(newDst: Register): Call = Call(newDst, name, args: _*)
}

// Don't use unless necessary.
case class Literal(instruction: String) extends Instruction {
	override def substitute(reg: Register, newValue: DefinedValue): Literal = Literal(instruction.replace(s" $reg ", s" $newValue "))
	def replaceDst(newDst: Register): Literal = Literal(instruction.replaceFirst("%\\w+(\\.\\w+)* = ", s"$newDst = "))
}

// Blocks

class Block(val name: String, var instructions: mutable.ArrayBuffer[Instruction]) {
	def this(name: String) = this(name, mutable.ArrayBuffer.empty)

	@targetName("appendOp")
	def += : Instruction => Unit = instructions.+=

	override def toString: String = name
	
	def removeNulls(): Unit = instructions.filterInPlace(_ != null)
}

// Functions

class Function(
	val name: String,
	val nameInLLVM: String,
	val returnType: CompilerType,
	val arguments: Seq[String],
	val argumentsInfo: mutable.HashMap[String, SymbolSourceInfo],
	val hostClass: Option[String],
	val nameGenerator: NameGenerator
) {
	val blocks: mutable.ArrayBuffer[Block] = mutable.ArrayBuffer.empty
	def nonNullBlocks: Seq[Block] = blocks.filterNot(_ == null).toSeq
	def nonNullBlockIndices: Seq[Int] = blocks.indices.filter(bIdx => blocks(bIdx) != null)
	private val blockNameToIndex: mutable.HashMap[String, Int] = mutable.HashMap.empty

	private val blockJumpsFrom: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]] = mutable.ArrayBuffer.empty
	private val blockJumpsTo: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]] = mutable.ArrayBuffer.empty

	def addBlock(name: Option[String] = None): Block = {
		val block = Block(s"${name.getOrElse(nameGenerator.nextLabel)}")
		blockNameToIndex.put(block.name, blocks.size)
		blocks.append(block)
		blockJumpsFrom.append(mutable.ArrayBuffer.empty)
		blockJumpsTo.append(mutable.ArrayBuffer.empty)
		block
	}

	def removeBlock(blockIdx: Int): Unit = {
		blockJumpsFrom(blockIdx).toSeq.foreach { blockTo => removeJump(blockIdx, blockTo) }
		blocks(blockIdx) = null
		blockJumpsFrom(blockIdx) = mutable.ArrayBuffer.empty
		blockJumpsTo.foreach(_.filterInPlace(_ != blockIdx))
	}

	def addJump(blockFrom: Int, blockTo: Int): Unit = {
		blockJumpsFrom(blockFrom).append(blockTo)
		blockJumpsTo(blockTo).append(blockFrom)
	}

	def addJump(blockFromName: String, blockToName: String): Unit =
		addJump(blockNameToIndex(blockFromName), blockNameToIndex(blockToName))

	def removeJump(blockFrom: Int, blockTo: Int): Unit = {
		if blocks(blockFrom) == null then return
		blockJumpsFrom(blockFrom).filterInPlace(_ != blockTo)
		blockJumpsTo(blockTo).filterInPlace(_ != blockFrom)
		val b = blocks(blockTo)
		if b != null then
			b.instructions.indices.foreach { iIdx =>
				val i = b.instructions(iIdx)
				if i != null then i match {
					case p: Phi => b.instructions(iIdx) = Phi(p.dst, p.cases.filter( _.blockName != blocks(blockFrom).name ): _*)
					case _ =>
				}
			}
	}

	def removeJump(blockFromName: String, blockToName: String): Unit =
		removeJump(blockNameToIndex(blockFromName), blockNameToIndex(blockToName))

	def collapseJump(blockFromName: String, blockToName: String): Unit = {
		val blockFrom = blockNameToIndex(blockFromName)
		val blockTo = blockNameToIndex(blockToName)
		// Copy instructions.
		blocks(blockFrom).instructions.dropRightInPlace(1).appendAll(blocks(blockTo).instructions)
		// Copy block jump information.
		blockJumpsFrom(blockFrom) = blockJumpsFrom(blockTo)
		// Clear removed block and its jump information.
		blocks(blockTo) = null
		blockJumpsTo(blockTo) = mutable.ArrayBuffer.empty
		blockJumpsFrom(blockTo) = mutable.ArrayBuffer.empty
		// Correct block jumps.
		for (blockIdx <- blockJumpsFrom(blockFrom)) do {
			blockJumpsTo(blockIdx).mapInPlace( j => if j == blockTo then blockFrom else j )
		}
		// Correct phi cases.
		for (blockIdx <- blockJumpsFrom(blockFrom); iIdx <- blocks(blockIdx).instructions.indices) do {
			blocks(blockIdx).instructions(iIdx) match {
				case p: Phi =>
					blocks(blockIdx).instructions(iIdx) = Phi(
						p.dst,
						p.cases.map { pc => PhiCase( if pc.blockName == blockToName then blockFromName else pc.blockName, pc.value) }: _*
					)
				case _ =>
			}
		}
	}

	def numBlocks: Int = blocks.size
	def getBlockIndex(block: Block): Int = blockNameToIndex(block.name)
	def getBlock(idx: Int): Block = blocks(idx)
	def getBlock(name: String): Block = blocks(blockNameToIndex(name))
	def getBlockJumpsFrom(idx: Int): Seq[Int] = blockJumpsFrom(idx).toSeq
	def getBlockJumpsTo(idx: Int): Seq[Int] = blockJumpsTo(idx).toSeq
	def getBlockJumpsFrom(name: String): Seq[Int] = getBlockJumpsFrom(blockNameToIndex(name))
	def getBlockJumpsTo(name: String): Seq[Int] = getBlockJumpsTo(blockNameToIndex(name))
	def getBlockName(idx: Int): String = blocks(idx).name
}
