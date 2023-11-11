package frontend.checks

import frontend.SymTable
import grammar.{LatteBaseVisitor, LatteParser}

object TopDefCollector extends LatteBaseVisitor[SymTable] {
	override def defaultResult(): SymTable = SymTable.createForLatte
	override def aggregateResult(aggregate: SymTable, nextResult: SymTable): SymTable = {
		aggregate.addAll(nextResult)
	}

	override def visitTopDef(ctx: LatteParser.TopDefContext): SymTable = {
		val name = ctx.ID.getText
		val topDefType = TypeCollector.visit(ctx)
		SymTable(name -> topDefType)
	}
}
