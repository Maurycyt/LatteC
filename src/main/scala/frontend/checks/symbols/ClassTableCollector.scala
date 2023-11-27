package frontend.checks.symbols

import frontend.checks.types.LatteType.TFunction
import grammar.{LatteBaseVisitor, LatteParser}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

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

		// Fills the class table up for a class and possibly its ancestors.
		def collectForClass(targetClass: String): Unit = {
			// If the class wasn't filled yet, fill it.
			// If the class has a parent, fill them first.
			if (resultTable.contains(targetClass)) return;
			hierarchyTable(targetClass)._2 match {
				case None =>
					resultTable.put(targetClass, (SymTable.empty, None))
				case Some(parentClass) =>
					collectForClass(parentClass)
					resultTable.put(targetClass, (resultTable(parentClass)._1.copy, Some(parentClass)))
			}

			// Now, join the members.
			hierarchyTable(targetClass)._1.memberDef.asScala.foreach { memberDef =>
				// If the member is a function and it already appears in the parent, then it must be replaced.
				MemberDefCollector.visit(memberDef).foreach {
					case (symbolName, symbolInfo @ SymbolInfo(_, t)) => t match {
						case _: TFunction => resultTable(targetClass)._1.put(symbolName, symbolInfo)
						case _ => resultTable(targetClass)._1.combineWith(symbolName, symbolInfo)
					}
				}
			}
		}

		classNames.foreach(collectForClass)
		resultTable
	}
}
