import frontend.checks.symbols.{ClassTable, SymTable}
import frontend.checks.symbols.{ClassTableCollector, TopDefCollector}
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
			exitWithError(f.frontendErrorToString)
	}
}
