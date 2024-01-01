package backend.generation

import frontend.checks.types.LatteType

import scala.collection.mutable

// Constants, Registers, and Labels

sealed trait Value { def valueType: LatteType }
sealed trait Address { def id: String }

case class Constant(value: Int, valueType: LatteType) extends Value
case class Register(id: String, valueType: LatteType) extends Address with Value
case class Label(id: String) extends Address

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

// Instructions

sealed trait Instruction

//case class Copy(dst: Register, arg: Value) extends Instruction
case class UnOp(dst: Register, op: UnaryOperator, arg: Value) extends Instruction
case class BinOp(dst: Register, arg1: Value, op: BinaryOperator, arg2: Value) extends Instruction
case class Jump(label: Label) extends Instruction
case class ConditionalJump(arg: Value, labelTrue: Label, labelFalse: Label) extends Instruction
case class GetElementPtr(dst: Register, ptr: Address, idx: Int, idxs: Int*) extends Instruction
case class PtrStore(ptr: Register, arg: Value) extends Instruction
case class PtrLoad(dst: Value, ptr: Register) extends Instruction
case object ReturnVoid extends Instruction
case class Return(arg: Value) extends Instruction
case class CallVoid(ptr: Address, args: Value*) extends Instruction
case class Call(dst: Register, ptr: Address, args: Value*) extends Instruction

// Blocks

class Block(val name: String, var instructions: mutable.ArrayBuffer[Instruction])

// Functions

class Function {
	val blocks: mutable.ArrayBuffer[Block] = mutable.ArrayBuffer.empty
	val blockNameToIndex: mutable.HashMap[String, Int] = mutable.HashMap.empty

	def addBlock(block: Block): Unit = {
		blockNameToIndex.put(block.name, blocks.size)
		blocks.append(block)
	}
}