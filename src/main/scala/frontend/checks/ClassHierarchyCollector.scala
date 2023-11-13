package frontend.checks

import grammar.{LatteBaseVisitor, LatteParser}

import scala.collection.mutable

/**
 * Collects information about how classes inherit after other classes.
 */
object ClassHierarchyCollector extends LatteBaseVisitor[ClassHierarchyCollector.InheritanceTable] {
	override val defaultResult: InheritanceTable = InheritanceTable.empty
	override def aggregateResult(aggregate: InheritanceTable, nextResult: InheritanceTable): InheritanceTable = aggregate.addAll(nextResult)

	override def visitDFun(ctx: LatteParser.DFunContext): InheritanceTable = defaultResult

	override def visitClassDef(ctx: LatteParser.ClassDefContext): InheritanceTable = {
		if ctx.ID.size > 1 then
			val name = ctx.ID(0).getText
			val parentName = ctx.ID(1).getText
			InheritanceTable(name -> parentName)
		else
			defaultResult
	}

	type InheritanceTable = mutable.HashMap[String, String]

	private object InheritanceTable {
		def empty: InheritanceTable = apply()
		def apply(inits: (String, String)*): InheritanceTable = mutable.HashMap.from(inits)
	}
}
