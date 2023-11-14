package frontend.checks.symbols

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
