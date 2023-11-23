package frontend.checks.symbols

import frontend.checks.symbols
import frontend.checks.types.TypeCollector
import grammar.LatteParser

/**
 * Collects the top-level definitions, which are meant to be available everywhere in the program.
 * Checks for duplicates and includes predefined functions.
 */
object TopDefCollector extends SymTableCollector {
	override def visitClassDef(ctx: LatteParser.ClassDefContext): SymTable = {
		val name = ctx.ID(0).getText
		val classDefType = TypeCollector.visitClassDef(ctx)
		val declarationPosition = Position(ctx.ID(0).getSymbol.getLine, ctx.ID(0).getSymbol.getCharPositionInLine + 1)
		SymTable(name -> symbols.SymbolInfo(declarationPosition, classDefType))
	}

	override def visitFunctionDef(ctx: LatteParser.FunctionDefContext): SymTable = {
		val name = ctx.ID.getText
		val funDefType = TypeCollector.visitFunctionDef(ctx)
		val declarationPosition = Position(ctx.ID.getSymbol.getLine, ctx.ID.getSymbol.getCharPositionInLine + 1)
		SymTable(name -> symbols.SymbolInfo(declarationPosition, funDefType))
	}

	override def visitProgram(ctx: LatteParser.ProgramContext): SymTable = {
		import SymTable.combineAll
		SymTable.withLattePredefined.combineAll(super.visitProgram(ctx))
	}
}
