package frontend.checks.types

import frontend.checks.symbols.*
import frontend.checks.RedeclarationError
import grammar.{LatteBaseVisitor, LatteParser}
import org.antlr.v4.runtime.tree.ParseTree

import scala.collection.mutable

/**
 * Checks if a parse tree is type-sound and returns its type.
 */
class TypeChecker(symTable: UnambiguousSymTable, classTable: UnambiguousClassTable) extends LatteBaseVisitor[LatteType] {
	private val scopeStack: mutable.Stack[mutable.Buffer[String]] = mutable.Stack.empty

	private def withNewScopeVisit(tree: ParseTree): Unit = {
		scopeStack.push(mutable.Buffer.empty)
		visit(tree)
		scopeStack.pop.foreach(symbolName => symTable.remove(symbolName))
	}

	private def addSymbol(symbolName: String, symbolInfo: UnambiguousSymbolInfo): Unit = {
		if (symTable.contains(symbolName)) {
			throw RedeclarationError(symbolInfo.declarationPosition, symbolName, symTable(symbolName).declarationPosition)
		}
		symTable += symbolName -> symbolInfo
		scopeStack.top += symbolName
	}

//	override def visitClassDef(ctx: LatteParser.ClassDefContext): LatteType = {
//
//	}
}
