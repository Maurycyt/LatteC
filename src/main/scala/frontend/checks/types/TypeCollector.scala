package frontend.checks.types

import LatteType.*
import grammar.{LatteBaseVisitor, LatteParser}

/**
 * Gets the type of a type specifier.
 */
object TypeCollector extends LatteBaseVisitor[LatteType] {
	override def visitTInt(ctx: LatteParser.TIntContext): LatteType = TInt
	override def visitTStr(ctx: LatteParser.TStrContext): LatteType = TStr
	override def visitTBool(ctx: LatteParser.TBoolContext): LatteType = TBool
	override def visitTVoid(ctx: LatteParser.TVoidContext): LatteType = TVoid

	override def visitTClass(ctx: LatteParser.TClassContext): LatteType = TClass(ctx.ID.getText)

	override def visitClassDef(ctx: LatteParser.ClassDefContext): LatteType = {
		TClass(ctx.ID(0).getText)
	}

	override def visitFunctionDef(ctx: LatteParser.FunctionDefContext): LatteType = {
		val returnType = visit(ctx.anyType).asInstanceOf[TNonFunction]
		val argTypes = if ctx.args == null then Seq.empty else
			(for childID <- 0 until ctx.args.getChildCount yield visit(ctx.args.getChild(childID)))
				.collect { case nonFunction: TNonFunction => nonFunction }
		TFunction(argTypes, returnType)
	}
}
