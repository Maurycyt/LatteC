package frontend.checks.symbols

import grammar.{LatteBaseVisitor, LatteParser}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * Collects information about the members of classes for the purpose of future lookup.
 */
object ClassTableCollector extends LatteBaseVisitor[ClassTable] {
	override def defaultResult: ClassTable = ClassTable.empty
	override def aggregateResult(aggregate: ClassTable, nextResult: ClassTable): ClassTable = aggregate.addAll(nextResult)

	override def visitFunctionDef(ctx: LatteParser.FunctionDefContext): ClassTable = defaultResult

	override def visitProgram(ctx: LatteParser.ProgramContext): ClassTable = {
		import SymTable.{combineWith, copy}

		val hierarchyTable = ClassHierarchyCollector.visitProgram(ctx)
		val classNames = hierarchyTable.keys
		val resultTable = ClassTable.empty

		def collectForClass(targetClass: String): Unit = {
			if (resultTable.contains(targetClass)) return;
			hierarchyTable(targetClass)._2 match {
				case None =>
					resultTable.put(targetClass, SymTable.empty)
				case Some(parentClass) =>
					collectForClass(parentClass)
					resultTable.put(targetClass, resultTable(parentClass).copy)
			}
			hierarchyTable(targetClass)._1.memberDef.asScala.foreach { memberDef =>
				MemberDefCollector.visitClassDef(hierarchyTable(targetClass)._1).foreach {
					(symbolName, symbolInfo) => resultTable(targetClass).combineWith(symbolName, symbolInfo)
				}
			}
		}

		classNames.foreach(collectForClass)
		resultTable
	}
}
