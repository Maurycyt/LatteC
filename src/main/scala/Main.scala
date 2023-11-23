import frontend.checks.symbols.{ClassTable, ClassTableCollector, SymTable, TopDefCollector}
import frontend.checks.FrontendError
import grammar.LatteParser
import grammar.LatteParser.ProgramContext
import parsing.ParseTreeGenerator

def exitWithError(message: String, status: Int = 1): Unit = {
	if (message.nonEmpty) {
		System.err.println(message)
	}
	System.exit(status)
}

@main
def main(inputFileString: String, debug: Boolean = false): Unit = {
	try {
		val program: ProgramContext = ParseTreeGenerator.getParseTree(inputFileString)
		val topDefSymbols: SymTable = TopDefCollector.visitProgram(program)
		val classSymbols: ClassTable = ClassTableCollector.visitProgram(program)
		println(s"There are ${topDefSymbols.size} top defs:")
		topDefSymbols.foreachEntry { (name, topDefType) => println(s"$name: $topDefType") }
	} catch {
		case ptg: ParseTreeGenerator.ParseTreeGeneratorException =>
			exitWithError(s"${ptg.getMessage}\nCause:\n${ptg.cause}")
		case f: FrontendError =>
			if (debug) f.printStackTrace()
			val fileReader = scala.io.Source.fromFile(inputFileString)
			exitWithError(s"""
					 |${f.frontendErrorToString}
					 ||${fileReader.getLines.drop(f.position.line - 1).next}
					 ||${" " * (f.position.col - 1)}^
					 |""".stripMargin)
	}
}
