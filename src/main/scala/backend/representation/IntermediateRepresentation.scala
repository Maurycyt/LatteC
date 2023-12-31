package backend.representation

import backend.generation.*
import frontend.checks.types.CompilerType

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

case class Undefined(valueType: CompilerType) extends Value { override def toStringWithoutType: String = "???" }
case class Constant(valueType: CompilerType, value: Long) extends DefinedValue { override def toStringWithoutType: String = value.toString }
case class Register(valueType: CompilerType, name: String) extends DefinedValue with Name
case class Label(valueType: CompilerType, name: String) extends Name

// Instructions

sealed trait Instruction {
	def substitute(reg: Register, newValue: DefinedValue): Instruction = this
}

// An assignment is an instruction which sets the value of a register.
sealed trait Assignment extends Instruction {
	def dst: Register
}


case class Copy(dst: Register, value: DefinedValue) extends Assignment {
	override def substitute(reg: Register, newValue: DefinedValue): Copy = if reg == value then copy(value = newValue) else this
}

case class PhiCase(blockName: String, value: DefinedValue) {
	def substitute(reg: Register, newValue: DefinedValue): PhiCase = if reg == value then copy(value = newValue) else this
}

case class Phi(dst: Register, cases: PhiCase*) extends Assignment {
	override def substitute(reg: Register, newValue: DefinedValue): Phi = Phi(dst, cases.map(_.substitute(reg, newValue)): _*)
}

case class BitcastStringConstant(dst: Register, stringConstant: String) extends Assignment

case class Bitcast(dst: Register, arg: Register) extends Assignment {
	override def substitute(reg: Register, newValue: DefinedValue): Bitcast = if reg == arg then copy(arg = newValue.asInstanceOf[Register]) else this
}

case class UnOp(dst: Register, op: UnaryOperator, arg: DefinedValue) extends Assignment {
	override def substitute(reg: Register, newValue: DefinedValue): UnOp = if reg == arg then copy(arg = newValue) else this
}

case class BinOp(dst: Register, arg1: DefinedValue, op: BinaryOperator, arg2: DefinedValue) extends Assignment {
	override def substitute(reg: Register, newValue: DefinedValue): BinOp = copy(
		arg1 = if arg1 == reg then newValue else arg1,
		arg2 = if arg2 == reg then newValue else arg2
	)
}

case class GetElementPtr(dst: Register, ptr: Name, idx: DefinedValue, idxs: DefinedValue*) extends Assignment {
	override def substitute(reg: Register, newValue: DefinedValue): GetElementPtr =
			GetElementPtr(
				dst,
				if ptr == reg then newValue.asInstanceOf[Register] else ptr,
				if idx == reg then newValue else idx,
				idxs.map { idx => if idx == reg then newValue else idx }: _*
			)
}

case class Jump(blockName: String) extends Instruction

case class ConditionalJump(arg: DefinedValue, blockNameTrue: String, blockNameFalse: String) extends Instruction {
	override def substitute(reg: Register, newValue: DefinedValue): ConditionalJump = if arg == reg then copy(arg = newValue) else this
}

case class PtrStore(ptr: Register, arg: DefinedValue) extends Instruction {
	override def substitute(reg: Register, newValue: DefinedValue): PtrStore = copy(
		if ptr == reg then newValue.asInstanceOf[Register] else ptr,
		if arg == reg then newValue else ptr
	)
}

case class PtrLoad(dst: Register, ptr: Register) extends Assignment {
	override def substitute(reg: Register, newValue: DefinedValue): PtrLoad = if ptr == reg then copy(ptr = newValue.asInstanceOf[Register]) else this
}

case object ReturnVoid extends Instruction

case class Return(arg: DefinedValue) extends Instruction {
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
}

// Blocks

class Block(val name: String, var instructions: mutable.ArrayBuffer[Instruction]) {
	def this(name: String) = this(name, mutable.ArrayBuffer.empty)

	@targetName("appendOp")
	def += : Instruction => Unit = instructions.+=
}

// Functions

class Function(
	val nameInLLVM: String,
	val returnType: CompilerType,
	val arguments: Seq[String],
	val argumentsInfo: mutable.HashMap[String, SymbolSourceInfo],
	val hostClass: Option[String],
	val nameGenerator: NameGenerator
) {
	private val blocks: mutable.ArrayBuffer[Block] = mutable.ArrayBuffer.empty
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
	
	def removeBlock(blockIdx: Int): Unit = blocks(blockIdx) = null

	def addJump(blockFrom: Int, blockTo: Int): Unit = {
		blockJumpsFrom(blockFrom).append(blockTo)
		blockJumpsTo(blockTo).append(blockFrom)
	}

	def addJump(blockFromName: String, blockToName: String): Unit = {
		addJump(blockNameToIndex(blockFromName), blockNameToIndex(blockToName))
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
