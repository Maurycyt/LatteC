package frontend.checks.types

import frontend.{FrontendError, Position}
import frontend.checks.symbols.*
import frontend.checks.types.LatteStmtType.*
import frontend.checks.types.LatteType.*
import grammar.{LatteBaseVisitor, LatteParser}
import grammar.LatteParser.{FunctionDefContext, MFunContext}
import org.antlr.v4.runtime.tree.ParseTree

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Checks if an expression is type sound and returns its type.
 */
class ExpressionTypeChecker()(using symbolStack: SymbolStack, classTable: ClassTable, currentClass: Option[TClass] = None) extends LatteBaseVisitor[LatteType] {
	import ClassTable.getClassOrThrow

	def matchExprTypeWithExpected(ctx: LatteParser.ExprContext | LatteParser.ValueContext, expectedTypes: Seq[LatteType]): LatteType = {
		val exprType: LatteType = visit(ctx)
		if expectedTypes.exists { expectedType => exprType.isSubtypeOf(expectedType) }
		then exprType
		else throw ExprTypeMismatchError(ctx, expectedTypes, exprType)
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

	override def visitERelOp(ctx: LatteParser.ERelOpContext): LatteType = {
		ctx.relOp.getText match {
			case "==" | "!=" =>
				val exprLeftType: LatteType = visit(ctx.expr(0))
				matchExprTypeWithExpected(ctx.expr(1), Seq(exprLeftType))
			case _ =>
				val exprLeftType: LatteType = matchExprTypeWithExpected(ctx.expr(0), Seq(TInt))
				matchExprTypeWithExpected(ctx.expr(1), Seq(exprLeftType))
		}
		TBool
	}

	private def visitLogOp(expr1: LatteParser.ExprContext, expr2: LatteParser.ExprContext): LatteType = {
		matchExprTypeWithExpected(expr1, Seq(TBool))
		matchExprTypeWithExpected(expr2, Seq(TBool))
	}
	override def visitEAnd(ctx: LatteParser.EAndContext): LatteType = visitLogOp(ctx.expr(0), ctx.expr(1))
	override def visitEOr(ctx: LatteParser.EOrContext): LatteType = visitLogOp(ctx.expr(0), ctx.expr(1))

	override def visitVSelf(ctx: LatteParser.VSelfContext): LatteType = currentClass match {
		case None => throw new FrontendError {
			override val position: Position = Position.fromToken(ctx.start)
			override val message: String = s"Usage of '${ctx.getText}' outside of the definition of a class is forbidden."
		}
		case Some(c) => c
	}

	override def visitVID(ctx: LatteParser.VIDContext): LatteType = {
		import SymbolStack.getOrThrow
		symbolStack.getOrThrow(ctx.ID.getText, Position.fromToken(ctx.ID.getSymbol)).symbolType
	}

	override def visitVMem(ctx: LatteParser.VMemContext): LatteType = {
		import ClassTable.getOrThrow
		visit(ctx.value) match {
			case TClass(className) => classTable.getOrThrow(className, ctx.ID.getText, Position.fromToken(ctx.ID.getSymbol)).symbolType
			case TArray(_) if ctx.ID.getText == "length" => TInt
			case nonClassType => throw ExprTypeMismatchError(ctx.value, Seq(TClass("<class>")), nonClassType)
		}
	}

	override def visitVArr(ctx: LatteParser.VArrContext): LatteType = {
		visit(ctx.value) match {
			case TArray(underlying) =>
				matchExprTypeWithExpected(ctx.expr, Seq(TInt))
				underlying
			case nonArray => throw ExprTypeMismatchError(ctx.value, Seq(TArray(TClass("<non array>"))), nonArray)
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
			case nonClassType => throw ExprTypeMismatchError(ctx, Seq(TClass("<class>")), nonClassType)
		}
	}

	override def visitENewArr(ctx: LatteParser.ENewArrContext): LatteType = {
		TypeCollector.visit(ctx.basicType) match {
			case classType @ TClass(className) =>
				classTable.getClassOrThrow(className, Position.fromToken(ctx.basicType.start))
				TArray(classType)
			case basic: TBasic => TArray(basic)
			case other => throw ExprTypeMismatchError(ctx, Seq(TClass("<non array>")), other)
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
			case other => throw ExprTypeMismatchError(ctx.value, Seq(TClass("<function>")), other)
		}
	}

	override def visitEParen(ctx: LatteParser.EParenContext): LatteType = visit(ctx.expr)
}

class StatementTypeChecker(
	currentExpectedReturnType: Option[LatteType] = None,
	currentClass: Option[TClass] = None
)(using
	private var symbolStack: SymbolStack,
	private val classTable: ClassTable
) extends LatteBaseVisitor[LatteStmtType] {
	given Option[TClass] = currentClass

	private case class UnreachableCodeError(position: Position) extends FrontendError {
		override def message: String = s"Unreachable code."
	}

	import ClassTable.getClassOrThrow
	import SymbolStack.getOrThrow

	private def withNewScopeDo(newScope: SymTable = SymTable.empty)(f : => LatteStmtType): LatteStmtType = {
		symbolStack = newScope :: symbolStack
		val result = f
		symbolStack = symbolStack.tail
		result
	}

	private def addSymbol(symbolName: String, symbolInfo: SymbolInfo): Unit = {
		import SymTable.combineWith
		symbolStack.head.combineWith(symbolName, symbolInfo)
	}

	override def visitClassDef(ctx: LatteParser.ClassDefContext): LatteStmtType = {
		val className = ctx.ID(0).getText
		val stmtCheckerInClass = StatementTypeChecker(currentExpectedReturnType, Some(TClass(className)))
		stmtCheckerInClass.withNewScopeDo(classTable(className)._1) {
			ctx.memberDef.asScala.filter { _.isInstanceOf[MFunContext] } .foreach(stmtCheckerInClass.visit)
			Ignored
		}
	}

	override def visitFunctionDef(ctx: LatteParser.FunctionDefContext): LatteStmtType = {
		// Collect the function signature.
		val functionName: String = ctx.ID.getText
		val functionType: TFunction = symbolStack.getOrThrow(functionName, Position.fromToken(ctx.ID.getSymbol)).symbolType.asInstanceOf[TFunction]
		val argTypes: Seq[LatteType] = functionType.args
		val args: SymTable =
			if argTypes.isEmpty
			then SymTable.empty
			else SymTable(argTypes.zipWithIndex.map { (argType, i) =>
				ctx.args.ID(i).getText -> SymbolInfo(Position.fromToken(ctx.args.ID(i).getSymbol), argType)
			}:_*)

		// Get the statement type with extended scope.
		val stmtCheckerInFunction = StatementTypeChecker(Some(functionType.result), currentClass)
		val stmtType = stmtCheckerInFunction.withNewScopeDo(args) { stmtCheckerInFunction.visitBlock(ctx.block) }

		// We do not need to confirm the type, because it is checked at the return statements.
		// If the statement loops, force void result.
		// Otherwise, throw that the function might not return.
		stmtType match {
			case result: MustReturn => result
			case wrongType: CanReturn if wrongType.latteType != functionType.result => throw StmtTypeMismatchError(ctx.block, functionType.result, wrongType.latteType)
			// We do not throw when code is unreachable after all
			/* case Loops =>
				if functionType.result == TVoid
				then Loops
				else throw new FrontendError {
					override def position: Position = Position.fromToken(ctx.block.start)
					override def message: String = s"Function $functionName loops and has return type ${functionType.result} but is expected to have return type $TVoid, because it never returns."
				} */
			case result =>
				if functionType.result == TVoid
				then result
				else throw new FrontendError {
					override def position: Position = Position.fromToken(ctx.block.start)
					override def message: String = s"Function $functionName is not guaranteed to return."
				}
		}
	}

	override def visitSEmpty(ctx: LatteParser.SEmptyContext): LatteStmtType = Ignored

	override def visitBlock(ctx: LatteParser.BlockContext): LatteStmtType = {
		withNewScopeDo(SymTable.empty) {
			ctx.stmt.asScala.foldLeft[LatteStmtType](Ignored) {
				// We do not throw when code is unreachable after all.
				// case (_: BreaksFlow, unreachableStmt) => throw UnreachableCodeError(Position.fromToken(unreachableStmt.start))
				case (previousResult, reachableStmt) =>
					val nextResult = visit(reachableStmt)
					(previousResult, nextResult) match {
						case (p: CanReturn, n: CanReturn) =>
							if p.latteType.isSubtypeOf(n.latteType) then n else
							if n.latteType.isSubtypeOf(p.latteType) then p else
							throw StmtTypeMismatchError(reachableStmt, p.latteType, n.latteType)
						case (result: CanReturn, _) => result
						case (_, result) => result
					}
			}
		}
	}

	override def visitDecl(ctx: LatteParser.DeclContext): LatteStmtType = {
		val itemType = TypeCollector.visit(ctx.anyType) match {
			case t @ TClass(className) => classTable.getClassOrThrow(className, Position.fromToken(ctx.anyType.start)); t
			case t @ TArray(TClass(className)) => classTable.getClassOrThrow(className, Position.fromToken(ctx.anyType.start)); t
			case t => t
		}
		ctx.item.asScala.foreach { itemCtx =>
			val itemSymbol = itemCtx.ID.getSymbol
			if (itemCtx.expr != null) ExpressionTypeChecker().matchExprTypeWithExpected(itemCtx.expr, Seq(itemType))
			addSymbol(itemSymbol.getText, SymbolInfo(Position.fromToken(itemSymbol), itemType))
		}
		Ignored
	}

	override def visitAss(ctx: LatteParser.AssContext): LatteStmtType = {
		val valueType = ExpressionTypeChecker().visit(ctx.value)
		ExpressionTypeChecker().matchExprTypeWithExpected(ctx.expr, Seq(valueType))
		Ignored
	}

	override def visitIncr(ctx: LatteParser.IncrContext): LatteStmtType = {
		ExpressionTypeChecker().matchExprTypeWithExpected(ctx.value, Seq(TInt))
		Ignored
	}

	override def visitDecr(ctx: LatteParser.DecrContext): LatteStmtType = {
		ExpressionTypeChecker().matchExprTypeWithExpected(ctx.value, Seq(TInt))
		Ignored
	}

	override def visitRetValue(ctx: LatteParser.RetValueContext): LatteStmtType = currentExpectedReturnType match {
		case None => throw new Error("No expected return type at return statement. This error should not appear.")
		case Some(expected) =>
			ExpressionTypeChecker().visit(ctx.expr) match {
				case good if good.isSubtypeOf(expected) => MustReturn(good)
				case bad => throw StmtTypeMismatchError(ctx, expected, bad)
			}
	}

	override def visitRetVoid(ctx: LatteParser.RetVoidContext): LatteStmtType = currentExpectedReturnType match {
		case None => throw new Error("No expected return type at return statement. This error should not appear.")
		case Some(TVoid) => MustReturn(TVoid)
		case Some(expected) => throw StmtTypeMismatchError(ctx, expected, TVoid)
	}

	override def visitCond(ctx: LatteParser.CondContext): LatteStmtType = {
		// Get the types of the expression and statement
		ExpressionTypeChecker().matchExprTypeWithExpected(ctx.expr, Seq(TBool))
		val stmtType = visit(ctx.stmt)

		// If the expression is false, throw unreachable.
		// If it is true, return whatever the statement returns.
		// If it is unknown, return whatever the statement returns with lower confidence.
		ctx.expr match {
			// We do not throw when code is unreachable after all.
			// case _: LatteParser.EFalseContext => throw UnreachableCodeError(Position.fromToken(ctx.stmt.start))
			case notFalse =>
				(notFalse, stmtType) match {
					case (_: LatteParser.ETrueContext, stmtType) => stmtType
					case (_, stmtType) => stmtType.loseConfidence
				}
		}
	}

	override def visitCondElse(ctx: LatteParser.CondElseContext): LatteStmtType = {
		// Get the types of the expression and statements
		ExpressionTypeChecker().matchExprTypeWithExpected(ctx.expr, Seq(TBool))
		val stmtTrueType = visit(ctx.stmt(0))
		val stmtFalseType = visit(ctx.stmt(1))

		// If the statement types don't match, throw.
		(stmtTrueType, stmtFalseType) match {
			case (trueCanReturn: CanReturn, falseCanReturn: CanReturn) if trueCanReturn.latteType != falseCanReturn.latteType =>
				throw StmtTypeMismatchError(ctx.stmt(1), trueCanReturn.latteType, falseCanReturn.latteType)
			case _ => ;
		}

		// If the expression is true or false, throw unreachable.
		// If it is unknown, consider the confidence of returning from each branch
		ctx.expr match {
			// We do not throw when code is unreachable after all.
			case _: LatteParser.EFalseContext => stmtFalseType // throw UnreachableCodeError(Position.fromToken(ctx.stmt(0).start))
			case _: LatteParser.ETrueContext => stmtTrueType // throw UnreachableCodeError(Position.fromToken(ctx.stmt(1).start))
			case notObvious => (stmtTrueType, stmtFalseType) match {
				case (MustReturn(resultType), MustReturn(_)) => MustReturn(resultType)
				case (Loops, Loops) => Loops
				case (_: DoesNotReturn, _: DoesNotReturn) => Ignored
				case (trueCanReturn: CanReturn, _) => trueCanReturn.loseConfidence
				case (_, falseCanReturn: CanReturn) => falseCanReturn.loseConfidence
			}
		}
	}

	override def visitWhile(ctx: LatteParser.WhileContext): LatteStmtType = {
		// Get the types of the expression and statement
		ExpressionTypeChecker().matchExprTypeWithExpected(ctx.expr, Seq(TBool))
		val stmtType = visit(ctx.stmt)

		// If the expression is false, throw unreachable.
		// If it is true and the statement does not return, return Loops
		// If it is true and the statement can return, return statement with lost confidence
		// If it is unknown, return statement with lost confidence
		(ctx.expr, stmtType) match {
//			case (_: LatteParser.EFalseContext, _) => throw UnreachableCodeError(Position.fromToken(ctx.stmt.start))
			case (_: LatteParser.ETrueContext, _: DoesNotReturn) => Loops
			case _ => stmtType.loseConfidence
		}
	}

	override def visitFor(ctx: LatteParser.ForContext): LatteStmtType = {
		// Get the types of the expression, iterator variable and statement with extended scope
		val iteratorType = TypeCollector.visit(ctx.basicType).asInstanceOf[TBasic]
		val iteratorName = ctx.ID.getText
		ExpressionTypeChecker().matchExprTypeWithExpected(ctx.expr, Seq(TArray(iteratorType)))
		val stmtType = withNewScopeDo(SymTable(iteratorName -> SymbolInfo(Position.fromToken(ctx.ID.getSymbol), iteratorType))) { visit(ctx.stmt) }

		// The loop might run over an array of size 0.
		// In this case we cannot assume that it breaks flow if the statement breaks flow.
		// We return the statement type with lost confidence.
		stmtType.loseConfidence
	}

	override def visitSExp(ctx: LatteParser.SExpContext): LatteStmtType = {
		ExpressionTypeChecker().visit(ctx.expr)
		Ignored
	}
}
