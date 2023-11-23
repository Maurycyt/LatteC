package frontend.checks.types

import frontend.checks.symbols.*
import frontend.checks.types.LatteType.*
import frontend.checks.FrontendError
import grammar.{LatteBaseVisitor, LatteParser}
import org.antlr.v4.runtime.tree.ParseTree

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Checks if an expression is type sound and returns its type.
 */
class ExpressionTypeChecker()(implicit symbolStack: SymbolStack, classTable: ClassTable) extends LatteBaseVisitor[LatteType] {
	import ClassTable.getClassOrThrow

	private def matchExprTypeWithExpected(ctx: LatteParser.ExprContext, expectedTypes: Seq[LatteType]): LatteType = {
		val exprType: LatteType = visit(ctx)
		if (!expectedTypes.contains(exprType)) throw TypeMismatchError(ctx, expectedTypes, exprType)
		exprType
	}

	override def visitEUnOp(ctx: LatteParser.EUnOpContext): LatteType = {
		val exprType: LatteType = visit(ctx.expr)
		ctx.unOp.getText match {
			case "-" => matchExprTypeWithExpected(ctx.expr, Seq(TInt))
			case "!" => matchExprTypeWithExpected(ctx.expr, Seq(TBool))
		}
	}

	override def visitEAddOp(ctx: LatteParser.EAddOpContext): LatteType = {
		val allowedTypes: Seq[LatteType] = if ctx.addOp().getText == "+" then Seq(TInt, TStr) else Seq(TInt)
		val exprLeftType: LatteType = matchExprTypeWithExpected(ctx.expr(0), allowedTypes)
		matchExprTypeWithExpected(ctx.expr(1), Seq(exprLeftType))
	}

	override def visitEMulOp(ctx: LatteParser.EMulOpContext): LatteType = {
		val exprLeftType: LatteType = matchExprTypeWithExpected(ctx.expr(0), Seq(TInt))
		matchExprTypeWithExpected(ctx.expr(1), Seq(exprLeftType))
	}

	override def visitERelOp(ctx: LatteParser.ERelOpContext): LatteType = ctx.relOp.getText match {
		case "==" | "!=" =>
			val exprLeftType: LatteType = visit(ctx.expr(0))
			matchExprTypeWithExpected(ctx.expr(1), Seq(exprLeftType))
		case _ =>
			val exprLeftType: LatteType = matchExprTypeWithExpected(ctx.expr(0), Seq(TInt))
			matchExprTypeWithExpected(ctx.expr(1), Seq(exprLeftType))
	}

	private def visitLogOp(expr1: LatteParser.ExprContext, expr2: LatteParser.ExprContext): LatteType = {
		matchExprTypeWithExpected(expr1, Seq(TBool))
		matchExprTypeWithExpected(expr2, Seq(TBool))
	}
	override def visitEAnd(ctx: LatteParser.EAndContext): LatteType = visitLogOp(ctx.expr(0), ctx.expr(1))
	override def visitEOr(ctx: LatteParser.EOrContext): LatteType = visitLogOp(ctx.expr(0), ctx.expr(1))

	override def visitVID(ctx: LatteParser.VIDContext): LatteType = {
		import SymbolStack.getOrThrow
		symbolStack.getOrThrow(ctx.ID.getText, Position.fromToken(ctx.ID.getSymbol)).symbolType
	}

	override def visitVMem(ctx: LatteParser.VMemContext): LatteType = {
		import ClassTable.getOrThrow
		visit(ctx.value) match {
			case TClass(className) => classTable.getOrThrow(className, ctx.ID.getText, Position.fromToken(ctx.ID.getSymbol)).symbolType
			case nonClassType => throw TypeMismatchError(ctx.value, Seq(TClass("<class>")), nonClassType)
		}
	}

	override def visitVArr(ctx: LatteParser.VArrContext): LatteType = {
		visit(ctx.value) match {
			case TArray(underlying) =>
				matchExprTypeWithExpected(ctx.expr, Seq(TInt))
				underlying
			case nonArray => throw TypeMismatchError(ctx.value, Seq(TArray(TClass("<non array>"))), nonArray)
		}
	}

	override def visitEInt(ctx: LatteParser.EIntContext): LatteType = TInt
	override def visitETrue(ctx: LatteParser.ETrueContext): LatteType = TBool
	override def visitEFalse(ctx: LatteParser.EFalseContext): LatteType = TBool
	override def visitEStr(ctx: LatteParser.EStrContext): LatteType = TStr

	override def visitENew(ctx: LatteParser.ENewContext): LatteType = {
		TypeCollector.visit(ctx.basicType) match {
			case classType @ TClass(className) =>
				classTable.getClassOrThrow(className, Position.fromToken(ctx.basicType.start))
				classType
			case nonClassType => throw TypeMismatchError(ctx.basicType, Seq(TClass("<class>")), nonClassType)
		}
	}

	override def visitENewArr(ctx: LatteParser.ENewArrContext): LatteType = {
		TypeCollector.visit(ctx.basicType) match {
			case classType @ TClass(className) =>
				classTable.getClassOrThrow(className, Position.fromToken(ctx.basicType.start))
				TArray(classType)
			case basic: TBasic => TArray(basic)
			case other => throw TypeMismatchError(ctx.basicType, Seq(TClass("<non array>")), other)
		}
	}

	override def visitENull(ctx: LatteParser.ENullContext): LatteType = {
		val className: String = ctx.ID.getText
		classTable.getClassOrThrow(className, Position.fromToken(ctx.ID.getSymbol))
		TClass(className)
	}

	override def visitEFunCall(ctx: LatteParser.EFunCallContext): LatteType = {
		visit(ctx.value) match {
			case TFunction(expectedArgTypes, resultType) =>
				val argExprs: Seq[LatteParser.ExprContext] = ctx.expr.asScala.toSeq
				if (expectedArgTypes.size != argExprs.size) throw new FrontendError {
					override val position: Position = Position.fromToken(argExprs.last.stop)
					override val message: String = s"Function applied to ${argExprs.size} arguments when ${expectedArgTypes.size} were expected."
				}
				argExprs.zip(expectedArgTypes).foreach { (expr, expectedType) => matchExprTypeWithExpected(expr, Seq(expectedType)) }
				resultType
			case other => throw TypeMismatchError(ctx.value, Seq(TClass("<function>")), other)
		}
	}
}

class StatementTypeChecker()(implicit private var symbolStack: SymbolStack, classTable: ClassTable) extends LatteBaseVisitor[LatteStmtType] {
	private def withNewScopeVisit(tree: ParseTree): Unit = {
		symbolStack = SymTable.empty :: symbolStack
		visit(tree)
		symbolStack = symbolStack.tail
	}

	private def addSymbol(symbolName: String, symbolInfo: SymbolInfo): Unit = {
		import SymTable.combineWith
		symbolStack.head.combineWith(symbolName, symbolInfo)
	}

//	override def visitDClass(ctx: LatteParser.DClassContext): LatteStmtType = {
//		val className = ctx.classDef.ID(0).getText
//	}
}
