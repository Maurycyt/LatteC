package frontend.checks

import frontend.{Position, SymbolInfo, SymTable}
import grammar.{LatteBaseVisitor, LatteParser}

object TopDefCollector extends LatteBaseVisitor[SymTable] {
	override def defaultResult(): SymTable = SymTable.withLattePredefined
	override def aggregateResult(aggregate: SymTable, nextResult: SymTable): SymTable = {
		aggregate.addAll(nextResult.collect {
			case sym @ (name, SymbolInfo(pos, _)) if pos != Position.predefined => if aggregate.contains(name) then throw RedeclarationError(pos, name) else sym
		})
	}

	override def visitClassDef(ctx: LatteParser.ClassDefContext): SymTable = {
		val name = ctx.ID(0).getText
		val classDefType = TypeCollector.visitClassDef(ctx)
		val declarationPosition = Position(ctx.ID(0).getSymbol.getLine, ctx.ID(0).getSymbol.getCharPositionInLine)
		SymTable(name -> SymbolInfo(declarationPosition, classDefType))
	}

	override def visitFunctionDef(ctx: LatteParser.FunctionDefContext): SymTable = {
		val name = ctx.ID.getText
		val funDefType = TypeCollector.visitFunctionDef(ctx)
		val declarationPosition = Position(ctx.ID.getSymbol.getLine, ctx.ID.getSymbol.getCharPositionInLine)
		SymTable(name -> SymbolInfo(declarationPosition, funDefType))
	}
}
