package backend.generation

import frontend.checks.types.LatteType.TStr
import grammar.{LatteBaseVisitor, LatteParser}

import java.io.FileWriter

object StringConstantGenerator {
	/**
	 * Generates LLVM IR code which defines the string constants in a program.
	 * @param fw The file writer to use when outputting LLVM IR code.
	 * @return A mapping from string to label to be referenced by other parts of the compiler when referring to string constants.
	 */
	def generateStringConstants(using ctx: LatteParser.ProgramContext, fw: FileWriter): Map[String, Label] = {
		val stringConstants: Seq[(String, Label)] =
			StringConstantCollector.visitProgram(ctx).toSeq
				.map(_.replace("\\n", "\\0A").appendedAll("\\00"))
				.zipWithIndex
				.map { (str, idx) => str -> Label(TStr, NamingConvention.stringConstant(idx)) }

		stringConstants.foreach { (str, label) =>
			fw write
				s"""${label.name} = internal constant [${str.length} x i8]
					 |\tc"$str"
					 |""".stripMargin
		}

		stringConstants.toMap
	}
}

object StringConstantCollector extends LatteBaseVisitor[Set[String]] {
	override def defaultResult(): Set[String] = Set.empty
	override def aggregateResult(aggregate: Set[String], nextResult: Set[String]): Set[String] = aggregate ++ nextResult

	override def visitEStr(ctx: LatteParser.EStrContext): Set[String] = Set(ctx.STR.getText)
}
