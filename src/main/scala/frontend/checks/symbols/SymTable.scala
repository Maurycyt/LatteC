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

		def getClassOrThrow(className: String, position: Position): SymTable = classTable.get(className) match {
			case Some(symTable) => symTable
			case None => throw SymbolNotFoundError(position, className)
		}

		def getOrThrow(className: String, symbolName: String, position: Position): SymbolInfo = classTable.get(className).flatMap(_.get(symbolName)) match {
			case Some(symbolInfo) => symbolInfo
			case None => throw SymbolNotFoundError(position, symbolName)
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
		def getOrThrow(symbolName: String, position: Position): SymbolInfo = {
			symbolStack.head.get(symbolName) match {
				case Some(result) => result
				case None => symbolStack match {
					case _ :: tail => tail.getOrThrow(symbolName, position)
					case Nil => throw SymbolNotFoundError(position, symbolName)
				}
			}
		}
	}
}