package frontend.checks.symbols

import frontend.{FrontendError, Position}
import frontend.checks.symbols.ClassHierarchyCollector.HierarchyTable
import grammar.{LatteBaseVisitor, LatteParser}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Collects information about the members of classes for the purpose of future lookup.
 */
class ClassTableCollector()(using hierarchyTable: HierarchyTable) extends LatteBaseVisitor[ClassTable] {
	override def defaultResult: ClassTable = ClassTable.empty
	override def aggregateResult(aggregate: ClassTable, nextResult: ClassTable): ClassTable = aggregate.addAll(nextResult)

	override def visitFunctionDef(ctx: LatteParser.FunctionDefContext): ClassTable = defaultResult

	override def visitProgram(ctx: LatteParser.ProgramContext): ClassTable = {
		given classNames: Set[String] = hierarchyTable.keys.toSet
		val resultTable = ClassTable.empty

		val classesInProgress: mutable.Set[String] = mutable.Set.empty

		// Fills the class table up for a class and possibly its ancestors.
		def collectForClass(targetClass: String): Unit = {
			// If the class is being filled, that means there is a cyclic hierarchy.
			if classesInProgress.contains(targetClass) then
				throw new FrontendError {
					override def position: Position = Position.fromToken(hierarchyTable(targetClass)._1.ID(0).getSymbol)
					override def message: String = s"Class $targetClass participates in cyclic inheritance, which is forbidden."
				}

			classesInProgress += targetClass

			// If the class wasn't filled yet, fill it.
			// If the class has a parent, fill them first.
			if (!resultTable.contains(targetClass)) {
				hierarchyTable(targetClass)._2 match {
					case None =>
						resultTable.put(targetClass, ClassTableEntry(MemberTable.empty, None))
					case Some(parentClass) =>
						if !hierarchyTable.contains(parentClass) then
							throw new FrontendError {
								override def position: Position = Position.fromToken(hierarchyTable(targetClass)._1.ID(1).getSymbol)
								override def message: String = s"Class $parentClass does not exist."
							}
						collectForClass(parentClass)
						resultTable.put(targetClass, ClassTableEntry(resultTable(parentClass).memberTable.copy, Some(parentClass)))
				}

				// Now, join the members.
				hierarchyTable(targetClass).defContext.memberDef.asScala.foreach { memberDef =>
					// If the member is a function and it already appears in the parent, then it must be replaced.
					MemberDefCollector().visit(memberDef).foreach {
						case (symbolName, symbolInfo) => resultTable(targetClass).memberTable.combineWith(symbolName, symbolInfo)
					}
				}
			}

			classesInProgress -= targetClass
		}

		classNames.foreach(collectForClass)
		resultTable
	}
}
