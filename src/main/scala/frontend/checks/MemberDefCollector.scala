package frontend.checks

import frontend.*
import grammar.LatteParser
import org.antlr.v4.runtime.tree.TerminalNode

import scala.jdk.CollectionConverters.*

/**
 * Collects the definitions of members in a class, both variables and functions.
 */
object MemberDefCollector extends SymTableCollector {
	override def defaultResult: SymTable = SymTable.empty

	override def visitMemberVariable(ctx: LatteParser.MemberVariableContext): SymTable = {
		import SymTable.combineWith
		val memberType: LatteType = TypeCollector.visit(ctx.anyType)
		val result: SymTable = SymTable.empty
		ctx.item.asScala.map { itemCtx =>
			(itemCtx.ID.getText, UnambiguousSymbolInfo(Position(itemCtx.ID.getSymbol.getLine, itemCtx.ID.getSymbol.getCharPositionInLine + 1), memberType))
		}.foreach { (symbolName, symbolInfo) => result.combineWith(symbolName, symbolInfo) }
		result
	}

	override def visitMemberFunction(ctx: LatteParser.MemberFunctionContext): SymTable = {
		val functionID: TerminalNode = ctx.functionDef.ID
		val methodType: LatteType = TypeCollector.visit(ctx.functionDef)
		val methodName: String = functionID.getText
		val position: Position = Position(functionID.getSymbol.getLine, functionID.getSymbol.getCharPositionInLine + 1)
		SymTable(methodName -> UnambiguousSymbolInfo(position, methodType))
	}
}
