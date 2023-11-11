package parsing

import grammar.{LatteLexer, LatteParser}
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, DiagnosticErrorListener, Parser}
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA

import java.util

object ParseTreeGenerator {
	case class ParseTreeGeneratorException(msg: String, cause: Throwable) extends Exception(msg, cause)

	private object AmbiguityListener extends DiagnosticErrorListener {
		override def reportContextSensitivity(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, prediction: Int, configs: ATNConfigSet): Unit = {}
		override def reportAttemptingFullContext(recognizer: Parser, dfa: DFA, startIndex: Int, stopIndex: Int, conflictingAlts: util.BitSet, configs: ATNConfigSet): Unit = {}
	}

	def getParseTree(filePathString: String): LatteParser.ProgramContext = {
		try {
			val inputStream = CharStreams.fromFileName(filePathString)
			val latteLexer = new LatteLexer(inputStream)
			val latteParser = new LatteParser(new CommonTokenStream(latteLexer))
			latteParser.addErrorListener(AmbiguityListener)
			latteParser.program
		} catch {
			case t: Throwable => throw ParseTreeGeneratorException("Could not parse the program.", t)
		}
	}
}
