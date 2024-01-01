import backend.generation.ClassRepresentationBuilder
import frontend.{FrontendError, Position}
import frontend.checks.symbols.*
import frontend.checks.symbols.ClassHierarchyCollector.HierarchyTable
import frontend.checks.types.LatteType.{TClass, TFunction, TInt}
import frontend.checks.types.StatementTypeChecker
import frontend.parsing.ParseTreeGenerator
import grammar.LatteParser
import grammar.LatteParser.ProgramContext
import org.apache.commons.io.FilenameUtils

import java.io.FileWriter
import java.nio.file.Path

def exitWithError(message: String, status: Int = 42): Unit = {
	if (message.nonEmpty) {
		System.err.println(message)
	}
	System.exit(status)
}

@main
def main(inputFileString: String, debugFlag: Boolean): Unit = {
	debug.flag = debugFlag
	val inputFilePath: Path = Path.of(inputFileString).toAbsolutePath

	import SymTable.classNames
	try {

		/***********
		| FRONTEND |
		***********/

		// Get symbols
		val program: ProgramContext = ParseTreeGenerator.getParseTree(inputFilePath)
		val topDefSymbols: SymTable = TopDefCollector(using Set.empty).visitProgram(program)
		given definedClassNames: Set[String] = topDefSymbols.classNames
		given symbolStack: SymbolStack[SymbolInfo] = SymbolStack(topDefSymbols.filter { (_, symbolInfo) => symbolInfo.symbolType match { case _: TClass => false; case _ => true }})
		given hierarchyTable: HierarchyTable = ClassHierarchyCollector.visitProgram(program)
		given classTable: ClassTable = ClassTableCollector().visitProgram(program)

		if debug.flag then println(s"Collected Class Table:\n$classTable")

		// Check types and flow.
		StatementTypeChecker().visitProgram(program)

		// Check if int main() is defined.
		{
			import SymbolTableExtension.getOrThrow
			val mainSymbol = topDefSymbols.getOrThrow("main", Position.fromToken(program.stop))
			val mainType = mainSymbol.symbolType
			val expectedMainType = TFunction(Seq.empty, TInt)
			if (mainType != expectedMainType) throw new FrontendError {
				override val position: Position = mainSymbol.declarationPosition
				override val message: String = s"Main symbol 'main' has type '$mainType' but is expected to have type '$expectedMainType'."
			}
		}

		System.err.println(s"${Console.BOLD}${Console.GREEN}OK!${Console.RESET}")

		/**********
		| BACKEND |
		**********/

		val inputFileBaseName: String = FilenameUtils.getBaseName(inputFileString)
		given fw: FileWriter(inputFilePath.resolveSibling(s"$inputFileBaseName.ll").toFile)

		// Generate class definitions.
		ClassRepresentationBuilder.buildClasses

		fw.close()

	} catch {

		case ptg: ParseTreeGenerator.ParseTreeGeneratorException =>
			exitWithError(s"${Console.BOLD}${Console.RED}WRONG!\n${ptg.getMessage}\nCause:\n${ptg.cause}${Console.RESET}")
		case f: FrontendError =>
			if (debugFlag) f.printStackTrace()
			val fileReader = scala.io.Source.fromFile(inputFileString)
			val line: String = fileReader.getLines.drop(f.position.line - 1).nextOption.getOrElse("")
			exitWithError(s"""
					 |${Console.BOLD}${Console.RED}WRONG!
					 |\t${f.frontendErrorToString}
					 |\t|$line
					 |\t|${line.collect { case '\t' => '\t'; case _ => ' ' } .take(f.position.col - 1)}^
					 |${Console.RESET}""".stripMargin)

	}
}
