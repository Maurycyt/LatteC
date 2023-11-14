package frontend.checks.symbols

case class Position(line: Int, col: Int) {
	override def toString: String = if this == Position.predefined then "<predefined>" else s"$line:$col"
}
object Position {
	val predefined: Position = Position(0, 0)
}
