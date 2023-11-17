package frontend.checks.symbols

import frontend.checks.symbols
import frontend.checks.types.TypeCollector
import grammar.LatteParser
import org.antlr.v4.runtime.tree.TerminalNode

import scala.jdk.CollectionConverters.*

/**
 * Collects the definitions of members in a class, both variables and functions.
 */
object MemberDefCollector extends SymTableCollector {
	override def visitMemberVariables(ctx: LatteParser.MemberVariablesContext): SymTable = {
		import SymTable.combineWith
		val memberType: LatteType = TypeCollector.visit(ctx.anyType)
		val result: SymTable = SymTable.empty
		ctx.item.asScala.map { itemCtx =>
			(itemCtx.ID.getText, symbols.UnambiguousSymbolInfo(Position(itemCtx.ID.getSymbol.getLine, itemCtx.ID.getSymbol.getCharPositionInLine + 1), memberType))
		}.foreach { (symbolName, symbolInfo) => result.combineWith(symbolName, symbolInfo) }
		result
	}

	override def visitMemberFunction(ctx: LatteParser.MemberFunctionContext): SymTable = {
		val functionID: TerminalNode = ctx.functionDef.ID
		val methodType: LatteType = TypeCollector.visit(ctx.functionDef)
		val methodName: String = functionID.getText
		val position: Position = Position(functionID.getSymbol.getLine, functionID.getSymbol.getCharPositionInLine + 1)
		SymTable(methodName -> symbols.UnambiguousSymbolInfo(position, methodType))
	}
}
