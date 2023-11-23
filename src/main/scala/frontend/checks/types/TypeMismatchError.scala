package frontend.checks.types

import frontend.checks.FrontendError
import frontend.checks.symbols.Position
import org.antlr.v4.runtime.ParserRuleContext

case class TypeMismatchError(position: Position, to: Position, expected: Seq[LatteType], actual: LatteType) extends FrontendError {
	val message: String = s"Expression from $position to $to is expected to have type ${expected.mkString("'", "' or '", "'")} but has type '$actual'."
}

object TypeMismatchError {
	def apply(ctx: ParserRuleContext, expected: Seq[LatteType], actual: LatteType): TypeMismatchError = {
		TypeMismatchError(Position.fromToken(ctx.start), Position.fromToken(ctx.stop), expected, actual)
	}
}
