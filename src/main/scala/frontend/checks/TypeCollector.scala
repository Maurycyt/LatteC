package frontend.checks

import frontend.LatteType
import frontend.LatteType._
import grammar.{LatteBaseVisitor, LatteParser}

object TypeCollector extends LatteBaseVisitor[LatteType] {
	override def visitInt(ctx: LatteParser.IntContext): LatteType = TInt
	override def visitStr(ctx: LatteParser.StrContext): LatteType = TStr
	override def visitBool(ctx: LatteParser.BoolContext): LatteType = TBool
	override def visitVoid(ctx: LatteParser.VoidContext): LatteType = TVoid

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
