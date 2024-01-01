package backend.generation

import frontend.checks.types.LatteType

import scala.collection.mutable

// Unary Operators

sealed trait UnaryOperator

case object Inv extends UnaryOperator
case object Neg extends UnaryOperator

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

// Constants, Registers, and Labels

sealed trait Source { def valueType: LatteType }

sealed trait Value extends Source
sealed trait DefinedValue extends Value
sealed trait Name extends Source { def name: String }

case class Undefined(valueType: LatteType) extends Value
case class Constant(valueType: LatteType, value: Int) extends DefinedValue
case class Register(valueType: LatteType, name: String) extends DefinedValue with Name
case class Label(valueType: LatteType, name: String) extends Name

// Instructions

sealed trait Instruction

case class UnOp(dst: Register, op: UnaryOperator, arg: Value) extends Instruction
case class BinOp(dst: Register, arg1: Value, op: BinaryOperator, arg2: Value) extends Instruction
case class GetElementPtr(dst: Register, ptr: Name, idx: Int, idxs: Int*) extends Instruction
case class GetSize(dst: Register, className: String, copies: Int)

case class Jump(label: Label) extends Instruction
case class ConditionalJump(arg: Value, labelTrue: Label, labelFalse: Label) extends Instruction
case class PtrStore(ptr: Register, arg: Value) extends Instruction
case class PtrLoad(dst: Register, ptr: Register) extends Instruction
case object ReturnVoid extends Instruction
case class Return(arg: Value) extends Instruction
case class CallVoid(ptr: Name, args: Value*) extends Instruction
case class Call(dst: Register, ptr: Name, args: Value*) extends Instruction

// Blocks

class Block(val name: String, var instructions: mutable.ArrayBuffer[Instruction]) {
	def this(name: String) = this(name, mutable.ArrayBuffer.empty)
}

// Functions

class Function(val name: String, val arguments: Seq[String], val argumentsInfo: mutable.HashMap[String, SymbolSourceInfo], val hostClass: Option[String]) {
	private val blocks: mutable.ArrayBuffer[Block] = mutable.ArrayBuffer.empty
	private val blockNameToIndex: mutable.HashMap[String, Int] = mutable.HashMap.empty

	def addBlock(block: Block): Unit = {
		blockNameToIndex.put(block.name, blocks.size)
		blocks.append(block)
	}

	def numBlocks: Int = blocks.size
	def getBlock(idx: Int): Block = blocks(idx)
	def getBlock(name: String): Block = blocks(blockNameToIndex(name))
}
