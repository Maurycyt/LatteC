package backend.generation

import backend.representation.{Constant, Function, Label, Register, ReturnVoid}
import frontend.checks.symbols.{ClassTable, SymbolStack}
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
				classTable(className).memberTable.asSymTable.map { (name, info) => name -> SymbolSourceInfo(info.symbolName, Some(className), Constant(info.symbolType, 0)) }
			),
			classTable,
			Some(className)
		).visitChildren(ctx)
	}

	override def visitFunctionDef(ctx: LatteParser.FunctionDefContext): Set[Function] = {
		// Prepare general function information.
		val functionName: String = ctx.ID.getText
		val (functionType, functionNameInLLVM) = hostClass match {
			case Some(className) =>
				val functionTypeWithoutSelf = TypeCollector().visitFunctionDef(ctx).asInstanceOf[TFunction]
				val functionTypeWithSelf = functionTypeWithoutSelf.copy(args = functionTypeWithoutSelf.args.prepended(TClass(className)))
				(functionTypeWithSelf, NamingConvention.method(className, functionName))
			case None =>
				(TypeCollector().visitFunctionDef(ctx).asInstanceOf[TFunction], NamingConvention.function(functionName))
		}
		val functionSourceInfo: SymbolSourceInfo = SymbolSourceInfo(functionName, hostClass, Label(functionType, functionNameInLLVM))
		val nameGenerator = NameGenerator()

		// Prepare function arguments.
		val argsWithTypes: Seq[(String, LatteType)] =
			(if ctx.args != null then
				ctx.args.ID.asScala.toSeq.zip(ctx.args.anyType.asScala.toSeq)
					.map { (id, anyType) => id.getText -> TypeCollector().visit(anyType) }
			else
				Seq.empty
			).prependedAll(
				hostClass match {
					case Some(className) => Some(NamingConvention.self -> TClass(className))
					case None => None
				}
			)

		val argScope =
			// The function itself, for recursion.
			mutable.HashMap(functionName -> functionSourceInfo) ++
			// The arguments.
			mutable.HashMap.from(
				argsWithTypes.map { (name, anyType) => name ->
					SymbolSourceInfo(
						name,
						None,
						if anyType != TVoid then
							Register(anyType, if name == NamingConvention.self then "%self" else nameGenerator.nextRegister)
						else
							Constant(anyType, 0))
				}
			)

		// Assemble function.
		val assembledFunction: Function = Function(functionName, functionNameInLLVM, functionType.result, argsWithTypes.filterNot(_._2 == TVoid).map(_._1), argScope, hostClass, nameGenerator)
		val entryBlock = assembledFunction.addBlock(Some("entry"))
		StatementAssembler.increaseReferenceCounts(
			argScope.collect { case (name, sourceInfo) if !Seq(NamingConvention.self, functionName).contains(name) => sourceInfo.source },
			assembledFunction,
			entryBlock
		)

		symbolStack.addScope(argScope)
		val (lastBlock, brokeFlow) = StatementAssembler(using symbolStack, classTable, assembledFunction, assembledFunction.getBlock(0), hostClass, None).visitBlock(ctx.block)
		if functionType.result == TVoid && !brokeFlow then
			StatementAssembler.decreaseReferenceCounts(
				argScope.collect { case (name, sourceInfo) if !Seq(NamingConvention.self, functionName).contains(name) => sourceInfo.source },
				assembledFunction,
				lastBlock
			)
			lastBlock += ReturnVoid
		symbolStack.removeScope()

		// Return function.
		Set(assembledFunction)
	}
}
