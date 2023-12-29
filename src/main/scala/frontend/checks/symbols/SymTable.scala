package frontend.checks.symbols

import frontend.{FrontendError, Position}
import frontend.checks.symbols
import frontend.checks.symbols.ClassHierarchyCollector.HierarchyTable
import frontend.checks.types.LatteType
import frontend.checks.types.LatteType.*

import scala.collection.immutable.HashSet
import scala.collection.mutable

// -------------------------------------
// |          Symbol Errors            |
// -------------------------------------

case class SymbolNotFoundError(position: Position, symbolName: String) extends FrontendError {
	val message: String = s"Symbol '$symbolName' not found."
}

case class RedeclarationError(position: Position, name: String, previousPosition: Position) extends FrontendError {
	override val message: String = s"Redeclaration of symbol '$name'. Previous declaration at $previousPosition."
}

case class RetypingError(position: Position, name: String, thisType: LatteType, previousType: LatteType, previousPosition: Position) extends FrontendError {
	override val message: String = s"Redeclaration of symbol '$name' overrides its type to '$thisType', which does not conform to the expected '$previousType' declared at $previousPosition."
}

// -------------------------------------
// |          Symbol Table             |
// -------------------------------------

case class SymbolInfo(declarationPosition: Position, symbolName: String, symbolType: LatteType, hostClass: Option[String] = None)

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
	).map { (name, symbolType) => (name, SymbolInfo(Position.predefined, name, symbolType)) }

	def withLattePredefined: SymTable = mutable.HashMap.from(LattePredefined)

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

		def classNames: Set[String] = HashSet.from(symTable.collect { case (_, SymbolInfo(_, _, TClass(name), _)) => name })
	}
}

// -------------------------------------
// |          Member Table             |
// -------------------------------------

case class MemberInfo(index: Int, symbolInfo: SymbolInfo)

given memberInfoOrdering: Ordering[MemberInfo] = (x: MemberInfo, y: MemberInfo) => x.index - y.index

class MemberTable(private var data: mutable.HashMap[String, MemberInfo] = mutable.HashMap.empty) {
	private var memberFunctions: Int = data.count { case (_, MemberInfo(_, SymbolInfo(_, _, symbolType, _))) => symbolType match { case _: TFunction => true; case _ => false } }
	private var memberVariables: Int = data.size - memberFunctions

	def apply: String => MemberInfo = data.apply
	def get: String => Option[MemberInfo] = data.get

	def combineWith(symbolName: String, symbolInfo: SymbolInfo)(using hierarchyTable: HierarchyTable = HierarchyTable.empty): MemberTable = {
		symbolInfo.symbolType match {
			case fType: TFunction =>
				data.get(symbolName).map(_.symbolInfo.symbolType) match {
					case None =>
						data += symbolName -> MemberInfo(memberFunctions, symbolInfo)
						memberFunctions += 1
					case Some(previousType) =>
						if fType.isSubtypeOf(previousType) then
							data += symbolName -> data(symbolName).copy(symbolInfo = symbolInfo)
						else
							throw RetypingError(symbolInfo.declarationPosition, symbolName, symbolInfo.symbolType, previousType, previousPosition = data(symbolName).symbolInfo.declarationPosition)
				}
			case _ =>
				if (data.contains(symbolName)) {
					throw RedeclarationError(symbolInfo.declarationPosition, symbolName, data(symbolName).symbolInfo.declarationPosition)
				}
				// We give variables indices starting at 1 because in a user-defined class, the vtable pointer appears first in the structure.
				data += symbolName -> MemberInfo(memberVariables + 1, symbolInfo)
				memberVariables += 1
		}
		this
	}

	def copy: MemberTable = new MemberTable(mutable.HashMap.from(data))

	def getOrThrow(memberName: String, position: Position): MemberInfo = data.get(memberName) match {
		case Some(memberInfo) => memberInfo
		case None => throw SymbolNotFoundError(position, memberName)
	}

	def asSymTable: SymTable = data.map { case (symbolName, memberInfo) => symbolName -> memberInfo.symbolInfo }

	// Sorted list of methods in order of definition in the hierarchy.
	def methods: Seq[MemberInfo] = data.values.filter { case MemberInfo(_, SymbolInfo(_, _, TFunction(_, _), _)) => true; case _ => false }.toSeq.sorted
	// Sorted list of fields in order of definition in the hierarchy.
	def fields: Seq[MemberInfo] = data.values.filterNot { case MemberInfo(_, SymbolInfo(_, _, TFunction(_, _), _)) => true; case _ => false }.toSeq.sorted

	override def toString: String = s"MemberTable($memberVariables,$memberFunctions,$data)"
}

object MemberTable {
	def empty: MemberTable = new MemberTable()

	def apply(inits: (String, SymbolInfo)*): MemberTable = {
		val result: MemberTable = empty
		for ((symbolName, symbolInfo) <- inits) {
			result.combineWith(symbolName, symbolInfo)
		}
		result
	}
}

// -------------------------------------
// |           Class Table             |
// -------------------------------------

// Each class has its members and possibly a parent class.

case class ClassTableEntry(memberTable: MemberTable, parent: Option[String])

type ClassTable = mutable.HashMap[String, ClassTableEntry]

object ClassTable {
	def empty: ClassTable = mutable.HashMap.empty
	def apply(inits: (String, ClassTableEntry)*): ClassTable = mutable.HashMap.from(inits)

	extension(classTable: ClassTable) {
		def copy: ClassTable = {
			classTable.map { case (className, ClassTableEntry(memberTable, parentName)) => className -> ClassTableEntry(memberTable.copy, parentName) }
		}

		def getClassOrThrow(className: String, position: Position): ClassTableEntry = classTable.get(className) match {
			case Some(symTableAndParent) => symTableAndParent
			case None => throw SymbolNotFoundError(position, className)
		}

		def getOrThrow(className: String, symbolName: String, position: Position): MemberInfo = classTable.get(className).flatMap(_.memberTable.get(symbolName)) match {
			case Some(memberInfo) => memberInfo
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