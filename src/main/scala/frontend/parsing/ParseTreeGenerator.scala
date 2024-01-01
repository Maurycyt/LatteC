package frontend.parsing

import frontend.{FrontendError, Position}
import grammar.{LatteLexer, LatteParser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, DiagnosticErrorListener, Parser, RecognitionException, Recognizer}
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA

import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import java.util

object ParseTreeGenerator {
	case class ParseTreeGeneratorException(msg: String, cause: Throwable) extends Exception(msg, cause)

	private object AmbiguityListener extends DiagnosticErrorListener {
		override def reportContextSensitivity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, prediction: Int, configs: ATNConfigSet): Unit = {}
		override def reportAttemptingFullContext(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, conflictingAlts: util.BitSet, configs: ATNConfigSet): Unit = {}
		override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: Any, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
			throw new FrontendError {
				override val position: Position = Position(line, charPositionInLine + 1)
				override val message: String = msg
				override val getCause: RecognitionException = e
			}
		}
	}

	def getParseTree(filePath: Path): LatteParser.ProgramContext = {
		try {
			val inputStream = CharStreams.fromPath(filePath)
			val latteLexer = new LatteLexer(inputStream)
			val latteParser = new LatteParser(new CommonTokenStream(latteLexer))
			latteLexer.removeErrorListeners()
			latteParser.removeErrorListeners()
			latteLexer.addErrorListener(AmbiguityListener)
			latteParser.addErrorListener(AmbiguityListener)
			latteParser.program
		} catch {
			case f: FrontendError => throw f
			case t: Throwable => throw ParseTreeGeneratorException("Could not parse the program.", t)
		}
	}
}
