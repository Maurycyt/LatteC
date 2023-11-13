package frontend.checks

import frontend.{ClassTable, SymTable, UnambiguousClassTable}
import grammar.{LatteBaseVisitor, LatteParser}

import scala.collection.mutable

/**
 * Collects information about the members of classes for the purpose of future lookup.
 */
object ClassMemberCollector extends LatteBaseVisitor[ClassTable] {
	override def defaultResult: ClassTable = ClassTable.empty
	override def aggregateResult(aggregate: ClassTable, nextResult: ClassTable): ClassTable = aggregate.addAll(nextResult)

	override def visitFunctionDef(ctx: LatteParser.FunctionDefContext): ClassTable = defaultResult

	override def visitClassDef(ctx: LatteParser.ClassDefContext): ClassTable = {
		val name: String = ctx.ID(0).getText
		val members: SymTable = MemberDefCollector.visitClassDef(ctx)
		ClassTable(name -> members)
	}

	def collectUnambiguous(ctx: LatteParser.ProgramContext): UnambiguousClassTable = {
		import ClassTable.unambiguous
		import SymTable.combineAll

		val classMembersNoInheritance = visitProgram(ctx)
		val classNames = classMembersNoInheritance.keys
		val inheritanceTable = ClassHierarchyCollector.visitProgram(ctx)
		val resultTable = mutable.HashMap.from(classNames.map { className => (className, SymTable.empty )})

		def closeUnderInheritance(targetClass: String, sourceClass: String): Unit = {
			if inheritanceTable.contains(sourceClass) then
				closeUnderInheritance(targetClass, inheritanceTable(sourceClass))
			else {
				resultTable.put(targetClass, SymTable.empty)
			}
			resultTable(targetClass).combineAll(classMembersNoInheritance(sourceClass))
		}

		classNames.foreach { className => closeUnderInheritance(className, className)}
		resultTable.unambiguous
	}
}
