package frontend

import frontend.LatteType._

import scala.collection.mutable

case class Position(line: Int, col: Int) {
	override def toString: String = s"$line:$col"
}
object Position { val predefined: Position = Position(0, 0) }
case class SymbolInfo(declarationPosition: Position, symbolType: LatteType)
type SymTable = mutable.HashMap[String, SymbolInfo]

object SymTable {
	def apply(inits: (String, SymbolInfo)*): SymTable = mutable.HashMap.from(inits)

	private val LattePredefined: Seq[(String, SymbolInfo)] = Seq(
		"printInt" -> TFunction(Seq(TInt), TVoid),
		"printString" -> TFunction(Seq(TStr), TVoid),
		"error" -> TFunction(Seq.empty, TVoid),
		"readInt" -> TFunction(Seq.empty, TInt),
		"readString" -> TFunction(Seq.empty, TStr)
	).map { (name, symbolType) => (name, SymbolInfo(Position.predefined, symbolType)) }

	def withLattePredefined: SymTable = mutable.HashMap.from(LattePredefined)
}
