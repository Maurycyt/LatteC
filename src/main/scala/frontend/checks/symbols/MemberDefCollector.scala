package frontend.checks.symbols

import frontend.checks.symbols
import frontend.checks.types.{LatteType, TypeCollector}
import grammar.{LatteBaseVisitor, LatteParser}
import org.antlr.v4.runtime.tree.TerminalNode

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Collects the definitions of members in a class, both variables and functions.
 */
object MemberDefCollector extends LatteBaseVisitor[mutable.Queue[(String, SymbolInfo)]] {
	private type returnType = mutable.Queue[(String, SymbolInfo)]
	override def defaultResult(): returnType = mutable.Queue.empty
	override def aggregateResult(aggregate: returnType, nextResult: returnType): returnType = aggregate.appendAll(nextResult)

	override def visitMemberVariables(ctx: LatteParser.MemberVariablesContext): returnType = {
		val memberType: LatteType = TypeCollector.visit(ctx.anyType)
		val result: returnType = mutable.Queue.empty
		ctx.ID.asScala.map { itemCtx =>
			(itemCtx.getText, symbols.SymbolInfo(Position(itemCtx.getSymbol.getLine, itemCtx.getSymbol.getCharPositionInLine + 1), memberType))
		}.foreach { (symbolName, symbolInfo) => result.append(symbolName -> symbolInfo) }
		result
	}

	override def visitMemberFunction(ctx: LatteParser.MemberFunctionContext): returnType = {
		val functionID: TerminalNode = ctx.functionDef.ID
		val methodType: LatteType = TypeCollector.visit(ctx.functionDef)
		val methodName: String = functionID.getText
		val position: Position = Position(functionID.getSymbol.getLine, functionID.getSymbol.getCharPositionInLine + 1)
		mutable.Queue(methodName -> symbols.SymbolInfo(position, methodType))
	}
}
