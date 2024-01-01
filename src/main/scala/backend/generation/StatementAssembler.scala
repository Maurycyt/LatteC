package backend.generation

import frontend.checks.symbols.SymbolStack
import grammar.{LatteBaseVisitor, LatteParser}

/**
 * Assembles statements in non-SSA form.
 * @param symbolStack The symbol stack to use when assembling a statement.
 * @param function The function which is being assembled.
 * @param thisBlock The block which is being assembled.
 * @param hostClass The possible host class of the function being assembled.
 * @param blockAfter The possible block which should be jumped to at the end of [[thisBlock]].
 */
class StatementAssembler(using symbolStack: SymbolStack[SymbolSourceInfo], function: Function, thisBlock: String, hostClass: Option[String], blockAfter: Option[String]) extends LatteBaseVisitor[Unit] {
	override def visitBlock(ctx: LatteParser.BlockContext): Unit = {
		symbolStack.addScope()
		visitChildren(ctx)
		symbolStack.removeScope()
	}


}
