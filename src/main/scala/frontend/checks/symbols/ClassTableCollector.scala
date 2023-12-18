package frontend.checks.symbols

import frontend.checks.types.LatteType.TFunction
import frontend.checks.types.{LatteType, StmtTypeMismatchError}
import frontend.{FrontendError, Position}
import grammar.{LatteBaseVisitor, LatteParser}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Collects information about the members of classes for the purpose of future lookup.
 */
class ClassTableCollector()(using classNames: Set[String]) extends LatteBaseVisitor[ClassTable] {
	override def defaultResult: ClassTable = ClassTable.empty
	override def aggregateResult(aggregate: ClassTable, nextResult: ClassTable): ClassTable = aggregate.addAll(nextResult)

	override def visitFunctionDef(ctx: LatteParser.FunctionDefContext): ClassTable = defaultResult

	override def visitProgram(ctx: LatteParser.ProgramContext): ClassTable = {
		import SymTable.{combineWith, copy}

		val hierarchyTable = ClassHierarchyCollector.visitProgram(ctx)
		val classNames = hierarchyTable.keys
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
						resultTable.put(targetClass, (SymTable.empty, None))
					case Some(parentClass) =>
						if !hierarchyTable.contains(parentClass) then
							throw new FrontendError {
								override def position: Position = Position.fromToken(hierarchyTable(targetClass)._1.ID(1).getSymbol)
								override def message: String = s"Class $parentClass does not exist."
							}
						collectForClass(parentClass)
						resultTable.put(targetClass, (resultTable(parentClass)._1.copy, Some(parentClass)))
				}

				// Now, join the members.
				hierarchyTable(targetClass)._1.memberDef.asScala.foreach { memberDef =>
					// If the member is a function and it already appears in the parent, then it must be replaced.
					MemberDefCollector().visit(memberDef).foreach {
						case (symbolName, symbolInfo@SymbolInfo(_, t)) => t match {
							case fType: TFunction =>
								val previousType: Option[LatteType] = resultTable(targetClass)._1.get(symbolName).map(_.symbolType)
								if previousType.isEmpty || fType.isSubtypeOf(previousType.get)(resultTable)
								then resultTable(targetClass)._1.put(symbolName, symbolInfo)
								else throw StmtTypeMismatchError(memberDef, previousType.get, fType)
							case _ => resultTable(targetClass)._1.combineWith(symbolName, symbolInfo)
						}
					}
				}
			}

			classesInProgress -= targetClass
		}

		classNames.foreach(collectForClass)
		resultTable
	}
}
