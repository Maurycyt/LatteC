package frontend.checks.symbols

import grammar.{LatteBaseVisitor, LatteParser}

import scala.collection.mutable

/**
 * Collects information about how classes inherit after other classes.
 */
object ClassHierarchyCollector extends LatteBaseVisitor[ClassHierarchyCollector.HierarchyTable] {
	override val defaultResult: HierarchyTable = HierarchyTable.empty
	override def aggregateResult(aggregate: HierarchyTable, nextResult: HierarchyTable): HierarchyTable = aggregate.addAll(nextResult)

	override def visitDFun(ctx: LatteParser.DFunContext): HierarchyTable = defaultResult

	override def visitClassDef(ctx: LatteParser.ClassDefContext): HierarchyTable = {
		val name = ctx.ID(0).getText
		if ctx.ID.size > 1 then
			val parentName = ctx.ID(1).getText
			HierarchyTable(name -> HierarchyTableEntry(ctx, Some(parentName)))
		else
			HierarchyTable(name -> HierarchyTableEntry(ctx, None))
	}

	case class HierarchyTableEntry(defContext: LatteParser.ClassDefContext, parent: Option[String])
	
	type HierarchyTable = mutable.HashMap[String, HierarchyTableEntry]

	object HierarchyTable {
		def empty: HierarchyTable = mutable.HashMap.empty
		def apply(inits: (String, HierarchyTableEntry)*): HierarchyTable = mutable.HashMap.from(inits)
	}
}
