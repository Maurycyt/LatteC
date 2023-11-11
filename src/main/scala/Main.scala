import frontend.checks.{FrontendError, TopDefCollector}
import frontend.SymTable
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
def main(inputFileString: String): Unit = {
	try {
		val program: ProgramContext = ParseTreeGenerator.getParseTree(inputFileString)
		val symTable: SymTable = TopDefCollector.visitProgram(program)
		println(s"There are ${symTable.size} top defs:")
		symTable.foreachEntry { (name, topDefType) => println(s"$name: $topDefType") }
	} catch {
		case ptg: ParseTreeGenerator.ParseTreeGeneratorException =>
			exitWithError(s"${ptg.getMessage}\nCause:\n${ptg.cause}")
		case f: FrontendError =>
			exitWithError(f.frontendErrorToString)
	}
}
