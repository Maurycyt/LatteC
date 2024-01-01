package frontend.checks.symbols

import frontend.{FrontendError, Position}
import frontend.checks.symbols
import frontend.checks.symbols.ClassHierarchyCollector.HierarchyTable
import frontend.checks.types.LatteType
import frontend.checks.types.LatteType.*

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.HashSet
import scala.collection.mutable

/****************
| Symbol Errors |
****************/

case class SymbolNotFoundError(position: Position, symbolName: String) extends FrontendError {
	val message: String = s"Symbol '$symbolName' not found."
}

case class RedeclarationError(position: Position, name: String, previousPosition: Position) extends FrontendError {
	override val message: String = s"Redeclaration of symbol '$name'. Previous declaration at $previousPosition."
}

case class RetypingError(position: Position, name: String, thisType: LatteType, previousType: LatteType, previousPosition: Position) extends FrontendError {
	override val message: String = s"Redeclaration of symbol '$name' overrides its type to '$thisType', which does not conform to the expected '$previousType' declared at $previousPosition."
}

/*******************
| Symbol Interface |
*******************/

/**
 * Contains all the necessary information about a symbol.
 */
trait SymbolInterface {
	/**
	 * @return The row and column ([[Position]] where the symbol was declared, used mostly in error reporting.
	 */
	def declarationPosition: Position

	/**
	 * @return The name of the symbol.
	 */
	def symbolName: String

	/**
	 * @return The type of the symbol
	 */
	def symbolType: LatteType
}

object SymbolTableExtension {
	extension [Info <: SymbolInterface](table: mutable.HashMap[String, Info]) {
		def combineWith(symbolName: String, symbolInfo: Info): mutable.HashMap[String, Info] = {
			if (table.contains(symbolName)) {
				throw RedeclarationError(symbolInfo.declarationPosition, symbolName, table(symbolName).declarationPosition)
			}
			table += symbolName -> symbolInfo
		}

		def combineAll(other: mutable.HashMap[String, Info]): mutable.HashMap[String, Info] = {
			for ((symbolName, symbolInfo) <- other) {
				combineWith(symbolName, symbolInfo)
			}
			table
		}

		def copy: mutable.HashMap[String, Info] = {
			mutable.HashMap.from(table)
		}

		def getOrThrow(symbolName: String, position: Position): Info = table.get(symbolName) match {
			case Some(info) => info
			case None => throw SymbolNotFoundError(position, symbolName)
		}
	}
}

import SymbolTableExtension.combineWith

/***************
| Symbol Table |
***************/

/**
 * Contains all the necessary information about a symbol in the context of static checks.
 * @param declarationPosition The row and column ([[Position]] where the symbol was declared, used mostly in error reporting.
 * @param symbolName The name of the symbol.
 * @param symbolType The type of the symbol.
 * @param hostClass The possible class in which the symbol was defined as a member.
 */
case class SymbolInfo(declarationPosition: Position, symbolName: String, symbolType: LatteType, hostClass: Option[String] = None) extends SymbolInterface

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
		def classNames: Set[String] = HashSet.from(symTable.collect { case (_, SymbolInfo(_, _, TClass(name), _)) => name })
	}
}

/***************
| Member Table |
***************/

/**
 * Contains all the necessary information about a symbol in the context of being a member of a class.
 * @param declarationPosition The row and column ([[Position]] where the symbol was declared, used mostly in error reporting.
 * @param symbolName The name of the symbol.
 * @param symbolType The type of the symbol.
 * @param hostClass The possible class in which the symbol was defined as a member.
 * @param offset The offset of the field in the memory of the object or the offset of the method pointer in the vtable.
 */
case class MemberInfo(declarationPosition: Position, symbolName: String, symbolType: LatteType, hostClass: String, offset: Int) extends SymbolInterface {
	def symbolInfo: SymbolInfo = SymbolInfo(declarationPosition, symbolName, symbolType, Some(hostClass))
}

object MemberInfo {
	def apply(symbolInfo: SymbolInfo, hostClass: String, offset: Int): MemberInfo =
		if !symbolInfo.hostClass.contains(hostClass) then throw RuntimeException("Host class does not match.")
		MemberInfo(symbolInfo.declarationPosition, symbolInfo.symbolName, symbolInfo.symbolType, hostClass, offset)
}

given memberInfoOrdering: Ordering[MemberInfo] = (x: MemberInfo, y: MemberInfo) => x.offset - y.offset

class MemberTable(val className: String, private var data: mutable.HashMap[String, MemberInfo] = mutable.HashMap.empty) {
	private var memberFunctions: Int = data.count { _._2.symbolType match { case _: TFunction => true; case _ => false } }
	private var memberVariables: Int = data.size - memberFunctions

	def apply: String => MemberInfo = data.apply
	def get: String => Option[MemberInfo] = data.get

	def combineWith(symbolName: String, symbolInfo: SymbolInfo)(using hierarchyTable: HierarchyTable = HierarchyTable.empty): MemberTable = {
		symbolInfo.symbolType match {
			case fType: TFunction =>
				data.get(symbolName).map(_.symbolType) match {
					case None =>
						data += symbolName -> MemberInfo(symbolInfo, className, memberFunctions)
						memberFunctions += 1
					case Some(previousType) =>
						if fType.isSubtypeOf(previousType) then
							data +=
								symbolName ->
								data(symbolName).copy(declarationPosition = symbolInfo.declarationPosition, symbolName = symbolInfo.symbolName, symbolType = symbolInfo.symbolType)
						else
							throw RetypingError(symbolInfo.declarationPosition, symbolName, symbolInfo.symbolType, previousType, previousPosition = data(symbolName).declarationPosition)
				}
			case _ =>
				if (data.contains(symbolName)) {
					throw RedeclarationError(symbolInfo.declarationPosition, symbolName, data(symbolName).declarationPosition)
				}
				// We give variables offsets starting at 1 because in a user-defined class, the vtable pointer appears first in the structure.
				data += symbolName -> MemberInfo(symbolInfo, className, memberVariables + 1)
				memberVariables += 1
		}
		this
	}

	def copy(newClassName: String = className): MemberTable = new MemberTable(newClassName, mutable.HashMap.from(data))

	def getOrThrow(memberName: String, position: Position): MemberInfo = data.get(memberName) match {
		case Some(memberInfo) => memberInfo
		case None => throw SymbolNotFoundError(position, memberName)
	}

	def asSymTable: SymTable = data.map { case (symbolName, memberInfo) => symbolName -> memberInfo.symbolInfo }

	// Sorted list of methods in order of definition in the hierarchy.
	def methods: Seq[MemberInfo] = data.values.filter { case MemberInfo(_, _, TFunction(_, _), _, _) => true; case _ => false }.toSeq.sorted
	// Sorted list of fields in order of definition in the hierarchy.
	def fields: Seq[MemberInfo] = data.values.filterNot { case MemberInfo(_, _, TFunction(_, _), _, _) => true; case _ => false }.toSeq.sorted

	def numFunctions: Int = memberFunctions
	def numVariables: Int = memberVariables

	override def toString: String = s"MemberTable($memberVariables,$memberFunctions,$data)"
}

/**************
| Class Table |
**************/

/**
 * The entry in a class table.
 * @param memberTable The table of members of a class.
 * @param parent The possible parent class of a class.
 */
case class ClassTableEntry(memberTable: MemberTable, parent: Option[String])

type ClassTable = mutable.HashMap[String, ClassTableEntry]

object ClassTable {
	def empty: ClassTable = mutable.HashMap.empty
	def apply(inits: (String, ClassTableEntry)*): ClassTable = mutable.HashMap.from(inits)

	extension(classTable: ClassTable) {
		def copy: ClassTable = {
			classTable.map { case (className, ClassTableEntry(memberTable, parentName)) => className -> ClassTableEntry(memberTable.copy(), parentName) }
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

/***************
| Symbol Stack |
***************/

class SymbolStack[Info <: SymbolInterface](inits: mutable.HashMap[String, Info]*) {
	private type Table = mutable.HashMap[String, Info]
	private var symTables: List[Table] = if inits.nonEmpty then List.from(inits) else mutable.HashMap.empty :: Nil
	private var symCounts: mutable.HashMap[String, Int] = mutable.HashMap.empty

	inits.foreach { _.keys.foreach { symbolName =>
		symCounts.updateWith(symbolName) { case None => Some(1); case Some(c) => Some(c + 1) }
	}}

	private def this(newSymTables: List[mutable.HashMap[String, Info]], newSymCounts: mutable.HashMap[String, Int]) = {
		this()
		symTables = newSymTables
		symCounts = mutable.HashMap.from(newSymCounts)
	}

	def add(symbolInfo: Info): Unit = {
		import SymbolTableExtension.combineWith
		symTables.head.combineWith(symbolInfo.symbolName, symbolInfo)
		symCounts.updateWith(symbolInfo.symbolName) { case None => Some(1); case Some(c) => Some(c + 1) }
	}

	@targetName("addInPlaceOp")
	def += : Info => Unit = add

	def count(symbolName: String): Int = symCounts.getOrElse(symbolName, 0)

	@tailrec
	private def getOrThrow(symTables: List[Table], symbolName: String, position: Position): Info = symTables match {
		case head :: tail =>
			head.get(symbolName) match {
				case Some(result) => result
				case None => getOrThrow(tail, symbolName, position)
			}
		case Nil => throw SymbolNotFoundError(position, symbolName)
	}

	def getOrThrow(symbolName: String, position: Position): Info = getOrThrow(symTables, symbolName, position)

	def withNewScope(scope: Table = mutable.HashMap.empty): SymbolStack[Info] = {
		SymbolStack[Info](scope :: symTables, symCounts)
	}

	def addScope(scope: Table = mutable.HashMap.empty): Unit = {
		symTables = scope :: symTables
		scope.keys.foreach{ symbolName =>
			symCounts.updateWith(symbolName) { case None => Some(1); case Some(c) => Some(c + 1) }
		}
	}

	def removeScope(): Unit = {
		val removedSymTable = symTables.head
		symTables = symTables.tail
		removedSymTable.keys.foreach { symbolName => symCounts.updateWith(symbolName){
			case Some(c) => Some(c-1)
			case None => throw RuntimeException("Symbol counts diverged when removing scope.")
		} }
	}
}
