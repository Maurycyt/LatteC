package backend.generation

import backend.representation.Label
import frontend.{FrontendError, Position}
import frontend.checks.types.LatteType.TStr
import grammar.{LatteBaseVisitor, LatteParser}

import java.io.FileWriter

object StringConstantGenerator {
	/**
	 * Generates LLVM IR code which defines the string constants in a program.
	 * @param fw The file writer to use when outputting LLVM IR code.
	 * @return A mapping from string to length and label to be referenced by
	 *         other parts of the compiler when referring to string constants.
	 */
	def generateStringConstants(using ctx: LatteParser.ProgramContext, fw: FileWriter): Map[String, (Int, Label)] = {
		val stringConstants: Seq[(String, (String, Int, Label))] =
			StringConstantCollector.visitProgram(ctx).toSeq
				.map { (str, pos) => str -> stringToLLVM(str, pos) }
				.zipWithIndex
				.map { case ((str, (strLLVM, lengthLLVM)), idx) => str -> (strLLVM, lengthLLVM, Label(TStr, NamingConvention.stringConstant(idx))) }

		stringConstants.foreach { case (_, (strLLVM, length, label)) =>
			fw write
				s"""${label.name} = internal constant [$length x i8]
					 |\tc"$strLLVM"
					 |""".stripMargin
		}

		stringConstants.map {case (str, (_, length, label)) => (str, (length, label))}.toMap
	}

	private def stringToLLVM(str: String, ctxPosition: Position): (String, Int) = {
		val strInLLVM = {
			val strWithoutLegalEscapes = str
				.replace("\\\\", "")
				.replace("\\\"", "")
				.replace("\\n", "")
				.replace("\\t", "")

			"\\\\[^nt\"\\\\]".r.findFirstIn(strWithoutLegalEscapes) match {
				case None => ()
				case Some(m) => throw new FrontendError {
					override def position: Position = ctxPosition
					override def message: String = s"String contains illegal escape sequence: '$m'."
				}
			}

			str
				.replace("\\\\", "\\5C")
				.replace("\\\"", "\\22")
				.replace("\\n", "\\0A")
				.replace("\\t", "\\09")
				.appendedAll("\\00")
		}
		val strLengthInLLVM = strInLLVM.length - 2 * "\\\\[0-9A-F]{2}".r.findAllIn(strInLLVM).length
		(strInLLVM, strLengthInLLVM)
	}
}

private object StringConstantCollector extends LatteBaseVisitor[Set[(String, Position)]] {
	override def defaultResult(): Set[(String, Position)] = Set.empty
	override def aggregateResult(aggregate: Set[(String, Position)], nextResult: Set[(String, Position)]): Set[(String, Position)] = aggregate ++ nextResult

	override def visitEStr(ctx: LatteParser.EStrContext): Set[(String, Position)] = {
		Set((ctx.STR.getText.stripPrefix("\"").stripSuffix("\""), Position.fromToken(ctx.STR.getSymbol)))
	}
}
