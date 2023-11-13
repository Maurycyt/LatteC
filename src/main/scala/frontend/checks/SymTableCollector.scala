package frontend.checks

import frontend.SymTable
import grammar.LatteBaseVisitor

/**
 * Common implementation of static symbol collectors.
 */
class SymTableCollector extends LatteBaseVisitor[SymTable] {
	override def aggregateResult(aggregate: SymTable, nextResult: SymTable): SymTable = {
		import SymTable.combineAll
		aggregate.combineAll(nextResult)
	}
}
