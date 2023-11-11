package frontend

import frontend.LatteType._

import scala.collection.mutable

type SymTable = mutable.HashMap[String, LatteType]

extension(symTable: SymTable) {
	def addSymbol(): (String, LatteType) => Unit = symTable.update
	def getType: String => LatteType = symTable.apply
}

object SymTable {
	def apply(inits: (String, LatteType)*): SymTable = mutable.HashMap.from(inits)

	private val LattePredefined: Seq[(String, LatteType)] = Seq(
		"printInt" -> TFunction(Seq(TInt), TVoid),
		"printString" -> TFunction(Seq(TStr), TVoid),
		"error" -> TFunction(Seq.empty, TVoid),
		"readInt" -> TFunction(Seq.empty, TInt),
		"readString" -> TFunction(Seq.empty, TStr)
	)

	def createForLatte: SymTable = mutable.HashMap.from(LattePredefined)
}
