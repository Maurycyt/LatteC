package backend.transcription

import backend.representation.*
import frontend.checks.types.CompilerType.PointerTo

import java.io.FileWriter

class Transcriber(
	using
	fw: FileWriter,
	stringMapping: Map[String, (Int, Label)]
) {
	def transcribeFunctions(functions: Set[Function]): Unit = {
		val functionsSeq = functions.toSeq
		if functionsSeq.nonEmpty then transcribeFunction(functionsSeq.head)
		for (i <- 1 until functionsSeq.size) do {
			fw write "\n\n"
			transcribeFunction(functionsSeq(i))
		}
	}

	private def transcribeFunction(function: Function): Unit = {
		fw write s"define ${function.returnType.toLLVM} ${function.nameInLLVM}("

		fw write function.arguments.map(function.argumentsInfo).map { argInfo =>
			s"${argInfo.symbolType} ${argInfo.source.asInstanceOf[Register].name}"
		}.mkString(", ")

		fw write ") nounwind {\n"

		transcribeBlock(function.getBlock("entry"))
		for (blockIdx <- 1 until function.numBlocks) do {
			fw write "\n"
			val block = function.getBlock(blockIdx)
			transcribeBlock(block)
		}

		fw write "}\n"
	}

	private def transcribeBlock(block: Block): Unit = {
		fw write s"${block.name}:\n"
		block.instructions.foreach { instr => fw write s"\t${transcribeInstruction(instr)}\n" }
	}

	private def transcribeInstruction(instr: Instruction): String = {
		instr match
			case Phi(dst, cases*) => s"$dst = phi ${dst.valueType.toLLVM} ${cases.map { phiCase => s"[ ${phiCase.value}, ${phiCase.blockName} ]"}.mkString(", ")}"
			case BitcastStringConstant(dst, stringConstant) => s"$dst = bitcast [${stringMapping(stringConstant)._1} x i8]* ${stringMapping(stringConstant)._2} to ${dst.valueType.toLLVM}"
			case Bitcast(dst, arg) => s"$dst = bitcast ${arg.valueType.toLLVM} $arg to ${dst.valueType.toLLVM}"
			case UnOp(dst, op, arg) => s"$dst = sub i${op match { case Inv => "64 0"; case Neg => "1 0" }}, $arg"
			case BinOp(dst, arg1, op, arg2) => s"$dst = " + (op match
				case Plus => s"add i64 $arg1, $arg2"
				case Minus => s"sub i64 $arg1, $arg2"
				case Mul => s"mul i64 $arg1, $arg2"
				case Div => s"sdiv i64 $arg1, $arg2"
				case Mod => s"sdiv i64 $arg1, $arg2"
				case Eq => s"icmp eq ${arg1.valueType.toLLVM} $arg1, $arg2"
				case Ne => s"icmp ne ${arg1.valueType.toLLVM} $arg1, $arg2"
				case Lt => s"icmp slt ${arg1.valueType.toLLVM} $arg1, $arg2"
				case Le => s"icmp sle ${arg1.valueType.toLLVM} $arg1, $arg2"
				case Gt => s"icmp sgt ${arg1.valueType.toLLVM} $arg1, $arg2"
				case Ge => s"icmp sge ${arg1.valueType.toLLVM} $arg1, $arg2"
				case And => s"and i1 $arg1, $arg2"
				case Or => s"or i1 $arg1, $arg2"
			)
			case GetElementPtr(dst, ptr, idx, idxs*) =>
				val idxArgs = idxs.prepended(idx).map { case c: Constant => s"i32 $c"; case r: Register => s"i64 $r" }.mkString(", ")
				s"$dst = getelementptr ${ptr.valueType.asInstanceOf[PointerTo].underlying.toLLVM}, ${ptr.valueType.toLLVM} $ptr, $idxArgs"
			case Jump(blockName) => s"br label %$blockName"
			case ConditionalJump(arg, blockNameTrue, blockNameFalse) => s"br i1 $arg, label %$blockNameTrue, label %$blockNameFalse"
			case PtrStore(ptr, arg) => s"store ${arg.toStringWithType}, ${ptr.toStringWithType}"
			case PtrLoad(dst, ptr) => s"$dst = load ${dst.valueType.toLLVM}, ${ptr.toStringWithType}"
			case ReturnVoid => "ret void"
			case Return(arg) => s"ret ${arg.toStringWithType}"
			case CallVoid(name, args*) => s"call void $name(${args.map(_.toStringWithType).mkString(", ")})"
			case Call(dst, name, args*) => s"$dst = call ${dst.valueType.toLLVM} $name(${args.map(_.toStringWithType).mkString(", ")})"
			case Copy(dst, value) => s"$dst = $value" //throw RuntimeException("Cannot transcribe Copy to LLVM IR.")
	}
}
