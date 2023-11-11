package frontend.checks

import frontend.LatteType
import frontend.LatteType._
import grammar.{LatteBaseVisitor, LatteParser}

object TypeCollector extends LatteBaseVisitor[LatteType] {
	override def visitInt(ctx: LatteParser.IntContext): LatteType = TInt
	override def visitStr(ctx: LatteParser.StrContext): LatteType = TStr
	override def visitBool(ctx: LatteParser.BoolContext): LatteType = TBool
	override def visitVoid(ctx: LatteParser.VoidContext): LatteType = TVoid

	override def visitTopDef(ctx: LatteParser.TopDefContext): LatteType = {
		val returnType = visit(ctx.type_).asInstanceOf[TNonFunction]
		val argTypes = if ctx.arg == null then Seq.empty else
			(for childID <- 0 until ctx.arg.getChildCount yield visit(ctx.arg.getChild(childID)))
				.collect { case nonFunction: TNonFunction => nonFunction }
		TFunction(argTypes, returnType)
	}
}
