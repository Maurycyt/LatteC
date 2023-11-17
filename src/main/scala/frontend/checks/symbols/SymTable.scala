package frontend.checks.symbols

import LatteType.*
import frontend.checks.{symbols, RedeclarationError}

import scala.collection.mutable

trait SymbolInfo {
	def declarationPosition: Position
	def combineWith(other: SymbolInfo): SymbolInfo
}
case class UnambiguousSymbolInfo(declarationPosition: Position, symbolType: LatteType) extends SymbolInfo {
	override def combineWith(other: SymbolInfo): SymbolInfo = other match {
		case UnambiguousSymbolInfo(pos, typ) =>
			if declarationPosition == pos
			then this
			else AmbiguousSymbolInfo(declarationPosition, pos)
		case AmbiguousSymbolInfo(pos, repos) =>
			if declarationPosition == pos
			then other
			else AmbiguousSymbolInfo(declarationPosition, pos)
	}
}
case class AmbiguousSymbolInfo(declarationPosition: Position, redeclarationPosition: Position) extends SymbolInfo {
	override def combineWith(other: SymbolInfo): SymbolInfo = this
}
type SymTable = mutable.HashMap[String, SymbolInfo]
type UnambiguousSymTable = mutable.HashMap[String, UnambiguousSymbolInfo]

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
	).map { (name, symbolType) => (name, symbols.UnambiguousSymbolInfo(Position.predefined, symbolType)) }

	def withLattePredefined: SymTable = mutable.HashMap.from(LattePredefined)

	extension(symTable: SymTable) {
		def combineWith(symbolName: String, symbolInfo: SymbolInfo): SymTable = {
			if symTable.contains(symbolName) then
				symTable.put(symbolName, symTable(symbolName).combineWith(symbolInfo))
			else
				symTable.put(symbolName, symbolInfo)
			symTable
		}

		def combineAll(other: SymTable): SymTable = {
			for ((symbolName, symbolInfo) <- other) { symTable.combineWith(symbolName, symbolInfo) }
			symTable
		}

		def unambiguous: UnambiguousSymTable = {
			symTable.map { (symbolName, symbolInfo) =>
				symbolName -> (symbolInfo match {
					case u: UnambiguousSymbolInfo => u
					case AmbiguousSymbolInfo(pos, repos) => throw RedeclarationError(repos, symbolName, pos)
				})
			}
		}

		def copy: SymTable = {
			mutable.HashMap.from(symTable)
		}
	}
}

type ClassTable = mutable.HashMap[String, SymTable]
type UnambiguousClassTable = mutable.HashMap[String, UnambiguousSymTable]

object ClassTable {
	def empty: ClassTable = mutable.HashMap.empty
	def apply(inits: (String, SymTable)*): ClassTable = mutable.HashMap.from(inits)

	extension(classTable: ClassTable)
		def unambiguous: UnambiguousClassTable = {
			classTable.map { (className, symTable) =>
				className -> SymTable.unambiguous(symTable)
			}
		}

		def copy: ClassTable = {
			classTable.map((className, classSymTable) => (className, SymTable.copy(classSymTable)))
		}
}
