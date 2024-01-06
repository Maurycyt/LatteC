package backend.generation

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
case object Neq extends BinaryOperator
case object Lt extends BinaryOperator
case object Le extends BinaryOperator
case object Gt extends BinaryOperator
case object Ge extends BinaryOperator
case object And extends BinaryOperator
case object Or extends BinaryOperator

object BinaryOperator {
	private val binOpMapping: Map[String, BinaryOperator] = Map(
		"+" -> Plus, "-" -> Minus, "*" -> Mul, "/" -> Div, "%" -> Mod,
		"==" -> Eq, "!=" -> Neq, "<" -> Lt, "<=" -> Le, ">" -> Gt, ">=" -> Ge,
		"&&" -> And, "||" -> Or
	)

	def from: String => BinaryOperator = binOpMapping.apply
}

// Constants, Registers, and Labels

sealed trait Source { def valueType: CompilerType }

sealed trait Value extends Source
sealed trait DefinedValue extends Value
sealed trait Name extends Source { def name: String }

case class Undefined(valueType: CompilerType) extends Value
case class Constant(valueType: CompilerType, value: Int) extends DefinedValue
case class Register(valueType: CompilerType, name: String) extends DefinedValue with Name
case class Label(valueType: CompilerType, name: String) extends Name

// Instructions

sealed trait Instruction

case class Copy(varName: String, value: DefinedValue) extends Instruction
case class PhiCase(blockName: String, value: DefinedValue)
case class Phi(dst: Register, cases: PhiCase*) extends Instruction

case class BitcastStringConstant(dst: Register, stringConstant: String) extends Instruction
case class Bitcast(dst: Register, arg: Register) extends Instruction

case class UnOp(dst: Register, op: UnaryOperator, arg: Value) extends Instruction
case class BinOp(dst: Register, arg1: Value, op: BinaryOperator, arg2: Value) extends Instruction
case class GetElementPtr(dst: Register, ptr: Name, idx: DefinedValue, idxs: DefinedValue*) extends Instruction

case class Jump(label: String) extends Instruction
case class ConditionalJump(arg: Value, labelTrue: String, labelFalse: String) extends Instruction
case class PtrStore(ptr: Register, arg: Value) extends Instruction
case class PtrLoad(dst: Register, ptr: Register) extends Instruction
case object ReturnVoid extends Instruction
case class Return(arg: Value) extends Instruction
case class CallVoid(ptr: Name, args: Value*) extends Instruction
case class Call(dst: Register, ptr: Name, args: Value*) extends Instruction

// Blocks

class Block(val name: String, var instructions: mutable.ArrayBuffer[Instruction]) {
	def this(name: String) = this(name, mutable.ArrayBuffer.empty)

	@targetName("appendOp")
	def += : Instruction => Unit = instructions.+=
}

// Functions

class Function(
	val nameInLLVM: String,
	val arguments: Seq[String],
	val argumentsInfo: mutable.HashMap[String, SymbolSourceInfo],
	val hostClass: Option[String],
	val nameGenerator: NameGenerator
) {
	private val blocks: mutable.ArrayBuffer[Block] = mutable.ArrayBuffer.empty
	private val blockNameToIndex: mutable.HashMap[String, Int] = mutable.HashMap.empty
	// There is a jump from A to B if and only if there exists and edge from B to A.
	private val blockGraph: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]] = mutable.ArrayBuffer.empty

	def addBlock(name: Option[String] = None): Block = {
		val block = Block(s"%${name.getOrElse(nameGenerator.nextLabel)}")
		blockNameToIndex.put(block.name, blocks.size)
		blocks.append(block)
		blockGraph.append(mutable.ArrayBuffer.empty)
		block
	}

	def addJump(blockFrom: Int, blockTo: Int): Unit = {
		blockGraph(blockTo).append(blockFrom)
	}

	def addJump(blockFromName: String, blockToName: String): Unit = {
		addJump(blockNameToIndex(blockFromName), blockNameToIndex(blockToName))
	}

	def numBlocks: Int = blocks.size
	def getBlock(idx: Int): Block = blocks(idx)
	def getBlock(name: String): Block = blocks(blockNameToIndex(name))
	private def getBlockJumps(idx: Int): Seq[Int] = blockGraph(idx).toSeq
	def getBlockJumps(name: String): Seq[Int] = getBlockJumps(blockNameToIndex(name))
	def getBlockName(idx: Int): String = blocks(idx).name
}
