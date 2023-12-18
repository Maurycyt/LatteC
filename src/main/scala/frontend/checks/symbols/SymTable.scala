package frontend.checks.symbols

import frontend.checks.types.LatteType.*
import frontend.checks.symbols
import frontend.checks.types.LatteType
import frontend.{FrontendError, Position}

import scala.collection.immutable.HashSet
import scala.collection.mutable

// -------------------------------------
// |          Symbol Info              |
// -------------------------------------

case class SymbolInfo(declarationPosition: Position, symbolType: LatteType)

// -------------------------------------
// |          Symbol Table             |
// -------------------------------------

case class SymbolNotFoundError(position: Position, symbolName: String) extends FrontendError {
	val message: String = s"Symbol '$symbolName' not found."
}

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

	private case class RedeclarationError(position: Position, name: String, previousPosition: Position) extends FrontendError {
		override val message: String = s"Redeclaration of symbol '$name'. Previous declaration at $previousPosition."
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

		def getOrThrow(symbolName: String, position: Position): SymbolInfo = symTable.get(symbolName) match {
			case Some(symbolInfo) => symbolInfo
			case None => throw SymbolNotFoundError(position, symbolName)
		}
		
		def classNames: Set[String] = HashSet.from(symTable.collect { case (_, SymbolInfo(_, TClass(name))) => name })
	}
}

// -------------------------------------
// |           Class Table             |
// -------------------------------------

// Each class has its symbols and possibly a parent class.
type ClassTable = mutable.HashMap[String, (SymTable, Option[String])]

object ClassTable {
	def empty: ClassTable = mutable.HashMap.empty
	def apply(inits: (String, (SymTable, Option[String]))*): ClassTable = mutable.HashMap.from(inits)

	extension(classTable: ClassTable) {
		def copy: ClassTable = {
			classTable.map { case (className, (classSymTable, parentName)) => className -> (SymTable.copy(classSymTable), parentName) }
		}

		def getClassOrThrow(className: String, position: Position): (SymTable, Option[String]) = classTable.get(className) match {
			case Some(symTableAndParent) => symTableAndParent
			case None => throw SymbolNotFoundError(position, className)
		}

		def getOrThrow(className: String, symbolName: String, position: Position): SymbolInfo = classTable.get(className).flatMap(_._1.get(symbolName)) match {
			case Some(symbolInfo) => symbolInfo
			case None => throw SymbolNotFoundError(position, symbolName)
		}

		def collectAncestors(className: String): List[TClass] = {
			def doCollectAncestors(className: Option[String]): List[TClass] = className match {
				case None => Nil
				case Some(c) => TClass(c) :: doCollectAncestors(classTable(c)._2)
			}
			doCollectAncestors(Some(className))
		}
	}
}

// -------------------------------------
// |          Symbol Stack             |
// -------------------------------------

type SymbolStack = List[SymTable]

object SymbolStack {
	def empty: SymbolStack = List.empty
	def apply(inits: SymTable*): SymbolStack = List.from(inits)

	extension(symbolStack: SymbolStack) {
		def getOrThrow(symbolName: String, position: Position): SymbolInfo = symbolStack match {
			case head :: tail =>
				head.get(symbolName) match {
					case Some(result) => result
					case None => tail.getOrThrow(symbolName, position)
				}
			case Nil => throw SymbolNotFoundError(position, symbolName)
		}
	}
}