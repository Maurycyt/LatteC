package backend.generation

import backend.representation.{Constant, Function, Label, Register}
import frontend.checks.symbols.{ClassTable, SymbolInterface, SymbolStack}
import frontend.checks.types.{LatteType, TypeCollector}
import frontend.checks.types.LatteType.{TClass, TFunction, TVoid}
import grammar.{LatteBaseVisitor, LatteParser}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Returns the set of all assembled functions in non-SSA form.
 * @param symbolStack           The symbol stack used when assembling a function.
 * @param classTable            The class table of the program.
 * @param hostClass             The possible host class of the function being assembled.
 */
class FunctionAssembler()(
	using
	symbolStack          : SymbolStack[SymbolSourceInfo],
	classTable           : ClassTable,
	hostClass            : Option[String]
) extends LatteBaseVisitor[Set[Function]] {
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
		if debug.flag then println(s"Assembling function $functionName.")
		val functionType: TFunction = TypeCollector().visitFunctionDef(ctx).asInstanceOf[TFunction]
		val functionNameInLLVM = hostClass match {
			case Some(className) => NamingConvention.method(className, functionName)
			case None => NamingConvention.function(functionName)
		}
		val functionSourceInfo: SymbolSourceInfo = SymbolSourceInfo(functionName, hostClass, Label(functionType, functionNameInLLVM))
		val nameGenerator = NameGenerator()

		val argsWithTypes: Seq[(String, LatteType)] =
			if ctx.args != null then
				ctx.args.ID.asScala.toSeq.zip(ctx.args.anyType.asScala.toSeq)
					.map { (id, anyType) => id.getText -> TypeCollector().visit(anyType) }
			else
				Seq.empty

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
					SymbolSourceInfo(name, None, if anyType != TVoid then Register(anyType, s"%${nameGenerator.nextRegister}") else Constant(anyType, 0))
				}
			)

		val assembledFunction: Function = Function(functionNameInLLVM, functionType.result, argsWithTypes.filterNot(_._2 == TVoid).map(_._1), newScope, hostClass, nameGenerator)
		assembledFunction.addBlock(Some("entry"))

		symbolStack.addScope(newScope)
		StatementAssembler(using symbolStack, classTable, assembledFunction, assembledFunction.getBlock(0), hostClass, None).visitBlock(ctx.block)
		symbolStack.removeScope()

		if debug.flag then println(s"Done with function $functionName.")

		Set(assembledFunction)
	}
}
