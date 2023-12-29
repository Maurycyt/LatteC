package frontend.checks.types

import frontend.checks.types.LatteType.*
import frontend.Position
import grammar.{LatteBaseVisitor, LatteParser}

import scala.jdk.CollectionConverters.*

/**
 * Gets the type of a type specifier.
 */
class TypeCollector(using classNames: Set[String] = Set.empty) extends LatteBaseVisitor[LatteType] {
	override def visitTInt(ctx: LatteParser.TIntContext): LatteType = TInt
	override def visitTStr(ctx: LatteParser.TStrContext): LatteType = TStr
	override def visitTBool(ctx: LatteParser.TBoolContext): LatteType = TBool
	override def visitTVoid(ctx: LatteParser.TVoidContext): LatteType = TVoid

	override def visitTClass(ctx: LatteParser.TClassContext): LatteType = {
		val name: String = ctx.ID.getText
		if classNames.contains(name)
		then TClass(name)
		else throw UnknownTypeError(Position.fromToken(ctx.start), TClass(name))
	}

	override def visitTArr(ctx: LatteParser.TArrContext): LatteType = TArray(visit(ctx.basicType).asInstanceOf[TNonFun])

	override def visitClassDef(ctx: LatteParser.ClassDefContext): LatteType = TClass(ctx.ID(0).getText)

	override def visitFunctionDef(ctx: LatteParser.FunctionDefContext): LatteType = {
		val returnType = visit(ctx.anyType)
		val argTypes =
			if ctx.args == null
			then Seq.empty
			else ctx.args.anyType.asScala.map { tCtx => visit(tCtx) }.toSeq
		TFunction(argTypes, returnType)
	}
}
