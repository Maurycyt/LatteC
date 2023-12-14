import frontend.checks.symbols.*
import frontend.checks.types.LatteType.{TClass, TFunction, TInt}
import frontend.checks.types.StatementTypeChecker
import frontend.parsing.ParseTreeGenerator
import frontend.{FrontendError, Position}
import grammar.LatteParser
import grammar.LatteParser.ProgramContext

def exitWithError(message: String, status: Int = 42): Unit = {
	if (message.nonEmpty) {
		System.err.println(message)
	}
	System.exit(status)
}

@main
def main(inputFileString: String, debug: Boolean): Unit = {
	try {
		// Get symbols
		val program: ProgramContext = ParseTreeGenerator.getParseTree(inputFileString)
		val topDefSymbols: SymTable = TopDefCollector.visitProgram(program)
		given symbolStack: SymbolStack = topDefSymbols.filter { (_, symbolInfo) => symbolInfo.symbolType match { case _: TClass => false; case _ => true }} :: Nil
		given classSymbols: ClassTable = ClassTableCollector.visitProgram(program)

		// Check types and flow.
		StatementTypeChecker().visitProgram(program)

		// Check if int main() is defined.
		{
			import SymTable.getOrThrow
			val mainSymbol = topDefSymbols.getOrThrow("main", Position.fromToken(program.stop))
			val mainType = mainSymbol.symbolType
			val expectedMainType = TFunction(Seq.empty, TInt)
			if (mainType != expectedMainType) throw new FrontendError {
				override val position: Position = mainSymbol.declarationPosition
				override val message: String = s"Main symbol 'main' has type '$mainType' but is expected to have type '$expectedMainType'."
			}
		}

		System.err.println(s"${Console.BOLD}${Console.GREEN}OK!${Console.RESET}")
	} catch {
		case ptg: ParseTreeGenerator.ParseTreeGeneratorException =>
			exitWithError(s"${Console.BOLD}${Console.RED}WRONG!\n${ptg.getMessage}\nCause:\n${ptg.cause}${Console.RESET}")
		case f: FrontendError =>
			if (debug) f.printStackTrace()
			val fileReader = scala.io.Source.fromFile(inputFileString)
			val line: String = fileReader.getLines.drop(f.position.line - 1).nextOption.getOrElse("")
			exitWithError(s"""
					 |${Console.BOLD}${Console.RED}WRONG!
					 |\t${f.frontendErrorToString}
					 |\t|$line
					 |\t|${line.collect { case '\t' => '\t' case _ => ' ' } .take(f.position.col - 1)}^
					 |${Console.RESET}""".stripMargin)
	}
}
