package frontend.checks.symbols

import frontend.checks.types.LatteType.*
import frontend.checks.{symbols, FrontendError}
import frontend.checks.types.LatteType

import scala.collection.mutable

// -------------------------------------
// |          Symbol Info              |
// -------------------------------------

case class SymbolInfo(declarationPosition: Position, symbolType: LatteType)

// -------------------------------------
// |          Symbol Table             |
// -------------------------------------

type SymTable = mutable.HashMap[String, SymbolInfo]

object SymTable {
	def empty: SymTable = mutable.HashMap.empty

	def apply(inits: (String, SymbolInfo)*): SymTable = {
		val result: SymTable = empty
		for ((symbolName, symbolInfo) <- inits) { result.combineWith(symbolName, symbolInfo) }
		result
	}

	private val LattePredefined: Seq[(String, SymbolInfo)] = Seq(
		"printInt" -> TFunction(Seq(TInt), TVoid),
		"printString" -> TFunction(Seq(TStr), TVoid),
		"error" -> TFunction(Seq.empty, TVoid),
		"readInt" -> TFunction(Seq.empty, TInt),
		"readString" -> TFunction(Seq.empty, TStr)
	).map { (name, symbolType) => (name, symbols.SymbolInfo(Position.predefined, symbolType)) }

	def withLattePredefined: SymTable = mutable.HashMap.from(LattePredefined)

	case class RedeclarationError(position: Position, name: String, previousPosition: Position) extends FrontendError {
		override def message: String = s"Redeclaration of symbol $name.\n\tNote: previous declaration at $previousPosition."
	}

	extension(symTable: SymTable) {
		def combineWith(symbolName: String, symbolInfo: SymbolInfo): SymTable = {
			if (symTable.contains(symbolName)) {
				throw RedeclarationError(symbolInfo.declarationPosition, symbolName, symTable(symbolName).declarationPosition)
			}
			symTable += symbolName -> symbolInfo
		}

		def combineAll(other: SymTable): SymTable = {
			for ((symbolName, symbolInfo) <- other) { symTable.combineWith(symbolName, symbolInfo) }
			symTable
		}

		def copy: SymTable = {
			mutable.HashMap.from(symTable)
		}
	}
}

// -------------------------------------
// |           Class Table             |
// -------------------------------------

type ClassTable = mutable.HashMap[String, SymTable]

object ClassTable {
	def empty: ClassTable = mutable.HashMap.empty
	def apply(inits: (String, SymTable)*): ClassTable = mutable.HashMap.from(inits)

	extension(classTable: ClassTable) {
		def copy: ClassTable = {
			classTable.map((className, classSymTable) => (className, SymTable.copy(classSymTable)))
		}
	}
}
