package frontend.checks.types

import frontend.{FrontendError, Position}
import grammar.LatteParser

case class ExprTypeMismatchError(position: Position, to: Position, expected: Seq[LatteType], actual: LatteType) extends FrontendError {
	val message: String = s"Expression from $position to $to has type '$actual' but is expected to have type ${expected.mkString("'", "' or '", "'")}."
}

object ExprTypeMismatchError {
	def apply(ctx: LatteParser.ExprContext | LatteParser.ValueContext, expected: Seq[LatteType], actual: LatteType): ExprTypeMismatchError = {
		ExprTypeMismatchError(Position.fromToken(ctx.start), Position.fromToken(ctx.stop), expected, actual)
	}
}

case class StmtTypeMismatchError(position: Position, to: Position, expected: LatteType, actual: LatteType) extends FrontendError {
	val message: String = s"Statement from $position to $to returns type '$actual' but is expected to return type '$expected'"
}

object StmtTypeMismatchError {
	def apply(ctx: LatteParser.StmtContext | LatteParser.BlockContext | LatteParser.MemberDefContext, expected: LatteType, actual: LatteType): StmtTypeMismatchError = {
		StmtTypeMismatchError(Position.fromToken(ctx.start), Position.fromToken(ctx.stop), expected, actual)
	}
}
