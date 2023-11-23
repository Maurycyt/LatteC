package frontend.checks.symbols

import org.antlr.v4.runtime.Token

case class Position(line: Int, col: Int) {
	override def toString: String = if this == Position.predefined then "<predefined>" else s"$line:$col"
}

object Position {
	val predefined: Position = Position(0, 0)
	def fromToken(token: Token): Position = Position(token.getLine, token.getCharPositionInLine + 1)
}
