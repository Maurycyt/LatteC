import backend.generation.{ClassRepresentationBuilder, FunctionAssembler, NamingConvention, PreambleGenerator, Normaliser, StringConstantGenerator, SymbolSourceInfo}
import backend.representation.{Function, Label}
import backend.transcription.Transcriber
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
import scala.sys.process.Process

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
		given program: ProgramContext = ParseTreeGenerator.getParseTree(inputFilePath)
		val topDefSymbols: SymTable = TopDefCollector(using Set.empty).visitProgram(program)
		given definedClassNames: Set[String] = topDefSymbols.classNames
		given topDefSymbolStack: SymbolStack[SymbolInfo] = SymbolStack(topDefSymbols.filter { (_, symbolInfo) => symbolInfo.symbolType match { case _: TClass => false; case _ => true }})
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
			if mainType != expectedMainType then throw new FrontendError {
				override val position: Position = mainSymbol.declarationPosition
				override val message: String = s"Main symbol 'main' has type '$mainType' but is expected to have type '$expectedMainType'."
			}
		}

		System.err.println(s"${Console.BOLD}${Console.GREEN}OK!${Console.RESET}")

		/**********
		| BACKEND |
		**********/

		val inputFileBaseName: String = FilenameUtils.getBaseName(inputFileString)
		def getPath(extension: String): Path = inputFilePath.resolveSibling(s"$inputFileBaseName.$extension")

		given fw: FileWriter(getPath("ll").toFile)

		// Generate preamble, string constants, and class definitions.
		PreambleGenerator.generatePreamble
		fw write "\n\n"
		val stringConstantMapping: Map[String, (Int, Label)] = StringConstantGenerator.generateStringConstants
		given Map[String, (Int, Label)] = stringConstantMapping
		fw write "\n\n"
		ClassRepresentationBuilder.buildClasses

		// Assemble the functions and transcribe them to LLVM IR.
		given topDefSymbolSourceStack: SymbolStack[SymbolSourceInfo] = SymbolStack(
			topDefSymbols.collect {
				case (symbolName, symbolInfo) if symbolInfo.symbolType.isInstanceOf[TFunction] =>
					val symbolNameInLLVM = if SymTable.LattePredefinedNames.contains(symbolName) then s"@$symbolName" else NamingConvention.function(symbolName)
					symbolName -> SymbolSourceInfo(symbolName, None, Label(symbolInfo.symbolType, symbolNameInLLVM))
			}
		)
		val functions: Set[Function] = FunctionAssembler()(using hostClass = None).visitProgram(program)
		functions.foreach { function => Normaliser.processFunction()(using function) }
		Transcriber().transcribeFunctions(functions)

		fw.close()

		// Finally, compile.
		Process(Seq("llvm-as", getPath("ll").toString, "-o", getPath("bc").toString)).!
		Process(Seq("llc", getPath("bc").toString, "-o", getPath("s").toString)).!
		Process(Seq("as", getPath("s").toString, "-o", getPath("o").toString)).!
		Process(Seq("clang", "src/main/resources/aux.c", "-c", "-o", "src/main/resources/aux.o")).!
		Process(Seq("clang", "-no-pie", "src/main/resources/aux.o", getPath("o").toString, "-o", inputFilePath.resolveSibling(inputFileBaseName).toString)).!
		if !debug.flag then Process(Seq("rm", getPath("ll").toString, getPath("bc").toString, getPath("s").toString, getPath("o").toString)).!

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
