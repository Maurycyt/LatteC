package frontend.checks.symbols

import frontend.checks.symbols
import frontend.checks.types.TypeCollector
import frontend.Position
import grammar.LatteParser

import scala.jdk.CollectionConverters.*

/**
 * Collects the top-level definitions, which are meant to be available everywhere in the program.
 * Checks for duplicates and includes predefined functions.
 */
class TopDefCollector(using classNames: Set[String]) extends SymTableCollector {
	override def visitClassDef(ctx: LatteParser.ClassDefContext): SymTable = {
		val name = ctx.ID(0).getText
		val classDefType = TypeCollector().visitClassDef(ctx)
		val declarationPosition = Position.fromToken(ctx.ID(0).getSymbol)
		SymTable(name -> symbols.SymbolInfo(declarationPosition, classDefType))
	}

	override def visitFunctionDef(ctx: LatteParser.FunctionDefContext): SymTable = {
		val name = ctx.ID.getText
		val funDefType = TypeCollector().visitFunctionDef(ctx)
		val declarationPosition = Position.fromToken(ctx.ID.getSymbol)
		SymTable(name -> symbols.SymbolInfo(declarationPosition, funDefType))
	}

	private def superVisitProgram(ctx: LatteParser.ProgramContext): SymTable = {
		import SymTable.combineAll
		SymTable.withLattePredefined.combineAll(super.visitProgram(ctx))
	}

	override def visitProgram(ctx: LatteParser.ProgramContext): SymTable = {
		val classNames: Set[String] = ctx.topDef.asScala.collect { case c: LatteParser.DClassContext => c.classDef.ID(0).getText }.toSet
		TopDefCollector(using classNames).superVisitProgram(ctx)
	}
}
