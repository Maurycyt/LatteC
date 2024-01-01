package backend.generation

import frontend.checks.symbols.{ClassTable, SymbolInterface, SymbolStack}
import frontend.checks.types.{LatteType, TypeCollector}
import frontend.checks.types.LatteType.{TClass, TVoid}
import grammar.{LatteBaseVisitor, LatteParser}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Returns the set of all assembled functions in non-SSA form.
 * @param symbolStack The symbol stack used when assembling a function.
 * @param classTable The class table of the program.
 * @param hostClass The possible host class of the function being assembled.
 */
class FunctionAssembler(using symbolStack: SymbolStack[SymbolSourceInfo], classTable: ClassTable, hostClass: Option[String]) extends LatteBaseVisitor[Set[Function]] {
	given classNames: Set[String] = classTable.keys.toSet

	override def defaultResult: Set[Function] = Set.empty
	override def aggregateResult(aggregate: Set[Function], nextResult: Set[Function]): Set[Function] = aggregate ++ nextResult

	override def visitClassDef(ctx: LatteParser.ClassDefContext): Set[Function] = {
		val className: String = ctx.ID(0).getText
		FunctionAssembler(using
			symbolStack.withNewScope(
				classTable(className).memberTable.asSymTable.map { (name, info) => name -> SymbolSourceInfo(info: SymbolInterface) }
			),
			classTable,
			Some(className)
		).visitChildren(ctx)
	}

	override def visitFunctionDef(ctx: LatteParser.FunctionDefContext): Set[Function] = {
		val functionName: String = ctx.ID.getText
		val functionType: LatteType = TypeCollector().visitFunctionDef(ctx)
		val functionSourceInfo: SymbolSourceInfo = hostClass match {
			case Some(className) => SymbolSourceInfo(functionName, hostClass, Label(functionType, NamingConvention.method(className, functionName)))
			case None => SymbolSourceInfo(functionName, hostClass, Label(functionType, NamingConvention.function(functionName)))
		}

		val argsWithTypes: Seq[(String, LatteType)] = ctx.args.ID.asScala.toSeq.zip(ctx.args.anyType.asScala.toSeq)
			.map { (id, anyType) => id.getText -> TypeCollector().visit(anyType) }

		val newScope =
			// The function itself, for recursion.
			mutable.HashMap(functionName -> functionSourceInfo) ++
			// The (somewhat artificial) self pointer, for access to members.
			{ hostClass match {
					case Some(className) => mutable.HashMap(NamingConvention.self -> SymbolSourceInfo(NamingConvention.self, None, Register(TClass(className), NamingConvention.self)))
					case None => mutable.HashMap.empty
			}} ++
			// The arguments.
			mutable.HashMap.from(
				argsWithTypes.map { (name, anyType) => name ->
					SymbolSourceInfo(name, None, if anyType != TVoid then Register(anyType, name) else Undefined(anyType))
				}
			)

		val assembledFunction: Function = Function(functionName, argsWithTypes.map(_._1), newScope, hostClass)
		assembledFunction.addBlock(Block("entry"))

		StatementAssembler(using symbolStack, assembledFunction, "entry", hostClass, None).visitBlock(ctx.block)

		Set(assembledFunction)
	}
}
