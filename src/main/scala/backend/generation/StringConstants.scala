package backend.generation

import backend.representation.Label
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
				.map { str => str -> str.stripPrefix("\"").stripSuffix("\"").replace("\\n", "\\0A").appendedAll("\\00") }
				.zipWithIndex
				.map { case ((str, strLLVM), idx) => str -> (strLLVM, str.length - 1, Label(TStr, NamingConvention.stringConstant(idx))) }

		stringConstants.foreach { case (_, (strLLVM, length, label)) =>
			fw write
				s"""${label.name} = internal constant [$length x i8]
					 |\tc"$strLLVM"
					 |""".stripMargin
		}

		stringConstants.map {case (str, (_, length, label)) => (str, (length, label))}.toMap
	}
}

object StringConstantCollector extends LatteBaseVisitor[Set[String]] {
	override def defaultResult(): Set[String] = Set.empty
	override def aggregateResult(aggregate: Set[String], nextResult: Set[String]): Set[String] = aggregate ++ nextResult

	override def visitEStr(ctx: LatteParser.EStrContext): Set[String] = Set(ctx.STR.getText)
}
