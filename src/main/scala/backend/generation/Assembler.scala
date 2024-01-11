package backend.generation

import backend.representation.*
import frontend.checks.symbols.{ClassTable, MemberInfo, SymbolStack}
import frontend.checks.types.LatteType
import frontend.checks.types.LatteType.*
import frontend.Position
import frontend.checks.types.CompilerType.{FunctionPointer, PointerTo}
import frontend.checks.types.TypeCollector
import grammar.{LatteBaseVisitor, LatteParser}
import org.antlr.v4.runtime.tree.ParseTree

import scala.jdk.CollectionConverters.*

case class GenerationError(message: String) extends Error

/**
 * Assembles statements in non-SSA form.
 * @param symbolStack   The symbol stack to use when assembling a statement.
 * @param classTable    Information about classes defined in the program.
 * @param function      The function which is being assembled.
 * @param thisBlock     The block which is being assembled.
 * @param hostClass     The possible host class of the function being assembled.
 * @param blockAfter    The possible block which should be jumped to at the end of [[thisBlock]].
 */
class StatementAssembler()(using
	symbolStack: SymbolStack[SymbolSourceInfo],
	classTable : ClassTable,
	function   : Function,
	thisBlock  : Block,
	hostClass  : Option[String],
	blockAfter : Option[Block]
) extends LatteBaseVisitor[(Block, Boolean)] {
	private def considerJumping(activeBlock: Block, brokeFlow: Boolean): (Block, Boolean) = {
		if brokeFlow then return (activeBlock, true)

		blockAfter match {
			case None => (activeBlock, false)
			case Some(block) =>
				activeBlock += Jump(block.name)
				function.addJump(activeBlock.name, block.name)
				(activeBlock, true)
		}
	}

	override def visitSEmpty(ctx: LatteParser.SEmptyContext): (Block, Boolean) = (thisBlock, false)

	override def visitBlock(ctx: LatteParser.BlockContext): (Block, Boolean) = {
		symbolStack.addScope()
		val stmts = ctx.stmt.asScala.toSeq
		val (activeBlock, brokeFlow) = stmts.foldLeft((thisBlock, false)) {
			case ((block, brokeFlow), stmt) =>
				if brokeFlow then
					(block, brokeFlow)
				else
					StatementAssembler()(using thisBlock = block, blockAfter = None).visit(stmt)
		}
		symbolStack.removeScope()
		considerJumping(activeBlock, brokeFlow)
	}

	override def visitSDecl(ctx: LatteParser.SDeclContext): (Block, Boolean) = {
		val itemType = TypeCollector(using classTable.keys.toSet).visit(ctx.anyType)
		val items = ctx.item.asScala.toSeq
		val activeBlock = items.foldLeft(thisBlock) { (block, item) => ItemAssembler(itemType)(using thisBlock = block).visitItem(item) }
		considerJumping(activeBlock, false)
	}

	override def visitSAss(ctx: LatteParser.SAssContext): (Block, Boolean) = {
		val (valueSource, writable, activeBlockV) = ValueAssembler().visit(ctx.value).asInstanceOf[(Register, Boolean, Block)]
		val (exprSource, activeBlock) = ExpressionAssembler()(using blocksAfter = None).visit(ctx.expr)

		// If it is writable, then we have a pointer.
		// If it is not writable, then we must be dealing with a local variable.
		if writable then
			activeBlock += PtrStore(valueSource, exprSource)
		else
			activeBlock += Copy(valueSource, exprSource)

		considerJumping(activeBlock, false)
	}

	private def visitPostcrement(ctx: LatteParser.SIncrContext | LatteParser.SDecrContext): (Block, Boolean) = {
		val ((valueSource, writable, activeBlock), op) = ctx match {
			case incr: LatteParser.SIncrContext => (ValueAssembler().visit(incr.value).asInstanceOf[(Register, Boolean, Block)], Plus)
			case decr: LatteParser.SDecrContext => (ValueAssembler().visit(decr.value).asInstanceOf[(Register, Boolean, Block)], Minus)
		}

		// If it is writable, then we have a pointer.
		// If it is not writable, then we must be dealing with a local variable.
		if writable then
			val tmpRegister1 = Register(TInt, s"%${function.nameGenerator.nextRegister}")
			val tmpRegister2 = Register(TInt, s"%${function.nameGenerator.nextRegister}")
			activeBlock += PtrLoad(tmpRegister1, valueSource)
			activeBlock += BinOp(tmpRegister2, tmpRegister1, op, Constant(TInt, 1))
			activeBlock += PtrStore(valueSource, tmpRegister2)
		else
			activeBlock += BinOp(valueSource, valueSource, op, Constant(TInt, 1))

		considerJumping(activeBlock, false)
	}
	override def visitSIncr(ctx: LatteParser.SIncrContext): (Block, Boolean) = visitPostcrement(ctx)
	override def visitSDecr(ctx: LatteParser.SDecrContext): (Block, Boolean) = visitPostcrement(ctx)

	override def visitSRetValue(ctx: LatteParser.SRetValueContext): (Block, Boolean) = {
		val (resultSource, activeBlock) = ExpressionAssembler()(using blocksAfter = None).visit(ctx.expr)
		if resultSource.valueType == TVoid then
			activeBlock += ReturnVoid
		else
			activeBlock += Return(resultSource)
		(activeBlock, true)
	}

	override def visitSRetVoid(ctx: LatteParser.SRetVoidContext): (Block, Boolean) = {
		thisBlock += ReturnVoid
		(thisBlock, true)
	}

	override def visitSCond(ctx: LatteParser.SCondContext): (Block, Boolean) = {
		val blockIfTrue = function.addBlock()
		val blockAfter = function.addBlock()

		ExpressionAssembler()(using blocksAfter = Some(blockIfTrue, blockAfter)).visit(ctx.expr)
		StatementAssembler()(using thisBlock = blockIfTrue, blockAfter = Some(blockAfter)).visit(ctx.stmt)

		considerJumping(blockAfter, false)
	}

	override def visitSCondElse(ctx: LatteParser.SCondElseContext): (Block, Boolean) = {
		val blockIfTrue = function.addBlock()
		val blockIfFalse = function.addBlock()
		val blockAfter = function.addBlock()

		ExpressionAssembler()(using blocksAfter = Some(blockIfTrue, blockIfFalse)).visit(ctx.expr)
		StatementAssembler()(using thisBlock = blockIfTrue, blockAfter = Some(blockAfter)).visit(ctx.stmt(0))
		StatementAssembler()(using thisBlock = blockIfFalse, blockAfter = Some(blockAfter)).visit(ctx.stmt(1))

		considerJumping(blockAfter, false)
	}

	override def visitSWhile(ctx: LatteParser.SWhileContext): (Block, Boolean) = {
		val condBlock = function.addBlock()
		val bodyBlock = function.addBlock()
		val blockAfter = function.addBlock()

		thisBlock += Jump(condBlock.name)
		function.addJump(thisBlock.name, condBlock.name)
		ExpressionAssembler()(using thisBlock = condBlock, blocksAfter = Some(bodyBlock, blockAfter)).visit(ctx.expr)
		StatementAssembler()(using thisBlock = bodyBlock, blockAfter = Some(condBlock)).visit(ctx.stmt)

		considerJumping(blockAfter, false)
	}

	override def visitSFor(ctx: LatteParser.SForContext): (Block, Boolean) = {
		???
	}

	override def visitSExp(ctx: LatteParser.SExpContext): (Block, Boolean) = {
		considerJumping(
			ExpressionAssembler()(using blocksAfter = blockAfter.map { b => (b, b) }).visit(ctx.expr)._2,
			false
		)
	}
}

/**
 * Assembles items in non-SSA form.
 * @param symbolStack The symbol stack to use when assembling a statement.
 * @param classTable  Information about classes defined in the program.
 * @param function    The function which is being assembled.
 * @param thisBlock   The block which is being assembled.
 * @param hostClass   The possible host class of the function being assembled.
 */
class ItemAssembler(
	itemType   : LatteType
)(
	using
	symbolStack: SymbolStack[SymbolSourceInfo],
	classTable : ClassTable,
	function   : Function,
	thisBlock  : Block,
	hostClass  : Option[String]
) extends LatteBaseVisitor[Block] {
	override def visitItem(ctx: LatteParser.ItemContext): Block = {
		val expr = ctx.expr
		val itemName = ctx.ID.getText
		if expr == null then
			itemType match {
				// TODO: Handle arrays?
				case _ =>
					val resultRegister = Register(itemType, s"%${function.nameGenerator.nextRegister}")
					thisBlock += Copy(resultRegister, Constant(itemType, 0))
					symbolStack.add(SymbolSourceInfo(itemName, None, resultRegister))
			}
			thisBlock
		else
			val (itemSource, activeBlock) = ExpressionAssembler()(using blocksAfter = None).visit(expr)
			val resultRegister = Register(itemSource.valueType, s"%${function.nameGenerator.nextRegister}")
			activeBlock += Copy(resultRegister, itemSource)
			symbolStack.add(SymbolSourceInfo(itemName, None, resultRegister))
			activeBlock
	}
}

/**
 * Assembles expressions in non-SSA form.
 * This visitor returns the Source of the result of the expression, and the active block where the source is to be used.
 * @param symbolStack    The symbol stack to use when assembling an expression.
 * @param classTable     Information about classes defined in the program.
 * @param function       The function which is being assembled.
 * @param thisBlock      The block which is being assembled.
 * @param hostClass      The possible host class of the function being assembled.
 * @param blocksAfter    The possible blocks which should be jumped to after computing the expression in the form of (ifTrue, ifFalse).
 */
class ExpressionAssembler()(
	using
	symbolStack: SymbolStack[SymbolSourceInfo],
	classTable : ClassTable,
	function   : Function,
	thisBlock  : Block,
	hostClass  : Option[String],
	blocksAfter: Option[(Block, Block)]
) extends LatteBaseVisitor[(DefinedValue, Block)] {

	private def considerJumping(jumpConditionSource: DefinedValue, activeBlock: Block): (DefinedValue, Block) = {
		blocksAfter match {
			case Some((jumpIfTrue, jumpIfFalse)) =>
				jumpConditionSource match {
					case Constant(TBool, 1)     =>
						activeBlock += Jump(jumpIfTrue.name)
						function.addJump(activeBlock.name, jumpIfTrue.name)
					case Constant(TBool, 0)     =>
						activeBlock += Jump(jumpIfFalse.name)
						function.addJump(activeBlock.name, jumpIfFalse.name)
					case r @ Register(TBool, _) =>
						if jumpIfTrue.name == jumpIfFalse.name then
							activeBlock += Jump(jumpIfTrue.name)
							function.addJump(activeBlock.name, jumpIfTrue.name)
						else
							activeBlock += ConditionalJump(jumpConditionSource, jumpIfTrue.name, jumpIfFalse.name)
							function.addJump(activeBlock.name, jumpIfTrue.name)
							function.addJump(activeBlock.name, jumpIfFalse.name)
					case unexpected             =>
						throw GenerationError(s"Unexpected jump condition case: $unexpected.")
			}
			case None => ()
		}
		(jumpConditionSource, activeBlock)
	}

	private def operateOnTwoIntegers(l: Long, op: String, r: Long): Constant = op match {
		case "+"  => Constant(TInt, l + r)
		case "-"  => Constant(TInt, l - r)
		case "*"  => Constant(TInt, l * r)
		case "/"  => Constant(TInt, l / r)
		case "%"  => Constant(TInt, l % r)
		case "==" => Constant(TBool, if l == r then 1 else 0)
		case "!=" => Constant(TBool, if l != r then 1 else 0)
		case "<"  => Constant(TBool, if l < r  then 1 else 0)
		case "<=" => Constant(TBool, if l <= r then 1 else 0)
		case ">"  => Constant(TBool, if l > r  then 1 else 0)
		case ">=" => Constant(TBool, if l >= r then 1 else 0)
	}

	override def visitEUnOp(ctx: LatteParser.EUnOpContext): (DefinedValue, Block) = ctx.unOp.getText match {
		case "-" =>
			// When taking the inverse, compute subexpression, then compute result.
			visit(ctx.expr) match {
				case (Constant(TInt, v), activeBlock) => (Constant(TInt, -v), activeBlock)
				case (r @ Register(TInt, _), activeBlock) =>
					val resultSource = Register(TInt, s"%${function.nameGenerator.nextRegister}")
					thisBlock += UnOp(resultSource, Inv, r)
					(resultSource, activeBlock)
				case unexpected => throw GenerationError(s"Unexpected EUnOp case: $unexpected.")
			}
		case "!" =>
			// When taking the negation, introduce negation to the context.
			blocksAfter match {
				case None =>
					val (subResultSource, activeBlock) = ExpressionAssembler().visit(ctx.expr)
					val resultSource = Register(TBool, s"%${function.nameGenerator.nextRegister}")
					activeBlock += UnOp(resultSource, Neg, subResultSource)
					(resultSource, activeBlock)
				case Some((jumpIfTrue, jumpIfFalse)) =>
					ExpressionAssembler()(using blocksAfter = Some(jumpIfFalse, jumpIfTrue)).visit(ctx.expr)
			}
	}

	override def visitEMulOp(ctx: LatteParser.EMulOpContext): (DefinedValue, Block) = {
		// Compute subexpressions.
		val notNull = visit(ctx.expr(0))
		val (subExpressionSourceL, activeBlockL) = notNull
		val (subExpressionSourceR, activeBlock) = ExpressionAssembler()(using thisBlock = activeBlockL).visit(ctx.expr(1))

		// Compute result.
		(subExpressionSourceL, ctx.mulOp.getText, subExpressionSourceR) match {
			case (Constant(TInt, l), op, Constant(TInt, r)) => (operateOnTwoIntegers(l, op, r), activeBlock)
			case (l: DefinedValue, op, r: DefinedValue) if l.valueType == TInt && r.valueType == TInt =>
				val resultSource = Register(TInt, s"%${function.nameGenerator.nextRegister}")
				thisBlock += BinOp(resultSource, l, BinaryOperator.from(op), r)
				(resultSource, activeBlock)
			case unexpected => throw GenerationError(s"Unexpected EMulOp case: $unexpected.")
		}
	}

	override def visitEAddOp(ctx: LatteParser.EAddOpContext): (DefinedValue, Block) = {
		// Compute subexpressions.
		val (subExpressionSourceL, activeBlockL) = visit(ctx.expr(0))
		val (subExpressionSourceR, activeBlock) = ExpressionAssembler()(using thisBlock = activeBlockL).visit(ctx.expr(1))

		// Compute result.
		(subExpressionSourceL, ctx.addOp.getText, subExpressionSourceR) match {
			case (Constant(TInt, l), op, Constant(TInt, r)) => (operateOnTwoIntegers(l, op, r), activeBlock)
			case (l: DefinedValue, op, r: DefinedValue) if l.valueType == TInt && r.valueType == TInt =>
				val resultSource = Register(TInt, s"%${function.nameGenerator.nextRegister}")
				thisBlock += BinOp(resultSource, l, BinaryOperator.from(op), r)
				(resultSource, activeBlock)
			case (l: Register, "+", r: Register) if l.valueType == TStr && r.valueType == TStr =>
				val resultSource = Register(TStr, s"%${function.nameGenerator.nextRegister}")
				thisBlock += Call(resultSource, Label(TFunction(Seq(TStr, TStr), TStr), "@concatenateStrings"), l, r)
				(resultSource, activeBlock)
			case unexpected => throw GenerationError(s"Unexpected EAddOp case: $unexpected.")
		}
	}

	override def visitERelOp(ctx: LatteParser.ERelOpContext): (DefinedValue, Block) = {
		// Get operation appropriate for the context.
		val operationToCompute = ctx.relOp.getText
		
		// Compute subexpressions forgetting about boolean negation.
		val (subExpressionSourceL, activeBlockL) = ExpressionAssembler()(using blocksAfter = None).visit(ctx.expr(0))
		val (subExpressionSourceR, activeBlock) = ExpressionAssembler()(using thisBlock = activeBlockL, blocksAfter = None).visit(ctx.expr(1))
		
		// Proceed as normal.
		lazy val resultRegister = Register(TBool, s"%${function.nameGenerator.nextRegister}")
		val resultSource = (subExpressionSourceL, operationToCompute, subExpressionSourceR) match {
			case (Constant(tl, l), op, Constant(tr, r)) if tl == tr && Seq(TInt, TBool, TVoid).contains(tl) => operateOnTwoIntegers(l, op, r)
			case (l: DefinedValue, op, r: DefinedValue) if l.valueType == TInt && r.valueType == TInt =>
				thisBlock += BinOp(resultRegister, l, BinaryOperator.from(op), r)
				resultRegister
			case (l: DefinedValue, op, r: DefinedValue) if Seq("==", "!=").contains(op) && l.valueType == r.valueType =>
				thisBlock += BinOp(resultRegister, l, BinaryOperator.from(op), r)
				resultRegister
			case unexpected => throw GenerationError(s"Unexpected ERelOp case: $unexpected.")
		}

		// If a jump is expected, jump.
		considerJumping(resultSource, activeBlock)
	}

	private enum LazyOp { case And, Or }

	override def visitEAnd(ctx: LatteParser.EAndContext): (DefinedValue, Block) = computeELazy(ctx.expr(0), ctx.expr(1), LazyOp.And)

	override def visitEOr(ctx: LatteParser.EOrContext): (DefinedValue, Block) = computeELazy(ctx.expr(0), ctx.expr(1), LazyOp.Or)

	private def computeELazy(lCtx: LatteParser.ExprContext, rCtx: LatteParser.ExprContext, op: LazyOp): (DefinedValue, Block) = {
		val (blockRight, blockIfTrue, blockIfFalse, blockWithResult) = blocksAfter match {
			case None =>
				// If there are no blocks given, then create your own block to save the result.
				(function.addBlock(), function.addBlock(), function.addBlock(), Some(function.addBlock()))
			case Some(ifTrue, ifFalse) =>
				// If blocks were given, then do not save the result, but jump to those given blocks instead.
				(function.addBlock(), ifTrue, ifFalse, None)
		}

		// Assemble the subexpressions. Propagate boolean negation.
		val (subExpressionSourceL, activeBlockL) = op match {
			case LazyOp.And => ExpressionAssembler()(using blocksAfter = Some(blockRight, blockIfFalse)).visit(lCtx)
			case LazyOp.Or => ExpressionAssembler()(using blocksAfter = Some(blockIfTrue, blockRight)).visit(lCtx)
		}
		val (subExpressionSourceR, activeBlockR) =
			ExpressionAssembler()(using thisBlock = blockRight, blocksAfter = Some(blockIfTrue, blockIfFalse)).visit(rCtx)

		// If the result was to be saved in a register, do it in a new block with Phi.
		// Otherwise, jumping was expected, and was already handled in the other blocks.
		blockWithResult match {
			case None =>
				// Return information about the last computation that was performed, after all other were inconclusive.
				(subExpressionSourceR, activeBlockR)
			case Some(activeBlock) =>
				blockIfTrue += Jump(activeBlock.name)
				blockIfFalse += Jump(activeBlock.name)
				val resultSource = Register(TBool, s"%${function.nameGenerator.nextRegister}")
				activeBlock += Phi(resultSource, PhiCase(blockIfTrue.name, Constant(TBool, 1)), PhiCase(blockIfFalse.name, Constant(TBool, 0)))
				(resultSource, activeBlock)
		}
	}

	override def visitEVal(ctx: LatteParser.EValContext): (DefinedValue, Block) = {
		val (resultSource, activeBlock) = ValueAssembler().visitForAccess(ctx).asInstanceOf[(DefinedValue, Block)]
		if resultSource.valueType == TBool then
			considerJumping(resultSource, activeBlock)
		else
			(resultSource, activeBlock)
	}

	override def visitEInt(ctx: LatteParser.EIntContext): (DefinedValue, Block) = {
		(Constant(TInt, ctx.INT.getText.toLong), thisBlock)
	}

	override def visitETrue(ctx: LatteParser.ETrueContext): (DefinedValue, Block) = 
		considerJumping(Constant(TBool, 1), thisBlock)

	override def visitEFalse(ctx: LatteParser.EFalseContext): (DefinedValue, Block) =
		considerJumping(Constant(TBool, 0), thisBlock)

	override def visitEStr(ctx: LatteParser.EStrContext): (DefinedValue, Block) = {
		val resultSource = Register(TStr, s"%${function.nameGenerator.nextRegister}")
		thisBlock += BitcastStringConstant(resultSource, StringConstantCollector.visit(ctx).head._1)
		(resultSource, thisBlock)
	}

	override def visitENew(ctx: LatteParser.ENewContext): (DefinedValue, Block) = {
		val className = ctx.basicType.getText
		val allocSource = Register(TStr, s"%${function.nameGenerator.nextRegister}")
		thisBlock += Call(allocSource, Label(TFunction(Seq(TInt), TStr), "@malloc"))
		val classPtr = Register(TClass(className), s"%${function.nameGenerator.nextRegister}")
		thisBlock += Bitcast(classPtr, allocSource)
		thisBlock += CallVoid(Label(TFunction(Seq(TClass(className)), TVoid), NamingConvention.constructor(className)))
		(classPtr, thisBlock)
	}

	override def visitENewArr(ctx: LatteParser.ENewArrContext): (DefinedValue, Block) = {
		???
	}

	override def visitENull(ctx: LatteParser.ENullContext): (DefinedValue, Block) = {
		val className = ctx.ID.getText
		(Constant(TClass(className), 0), thisBlock)
	}

	override def visitEFunCall(ctx: LatteParser.EFunCallContext): (DefinedValue, Block) = {
		val (functionPtr, activeBlockF) = ValueAssembler().visitForAccess(ctx.value).asInstanceOf[(Name, Block)]
		var activeBlock = activeBlockF
		val exprs: Seq[LatteParser.ExprContext] = if ctx.expr == null then Seq.empty[LatteParser.ExprContext] else ctx.expr.asScala.toSeq

		val exprSources: Seq[DefinedValue] = exprs.map { expr =>
			val (exprSource, activeBlockE) = ExpressionAssembler()(using blocksAfter = None).visit(expr)
			activeBlock = activeBlockE
			exprSource
		}.filterNot(_.valueType == TVoid)

		functionPtr.valueType.asInstanceOf[TFunction] match {
			case TFunction(_, TVoid) =>
				activeBlock += CallVoid(functionPtr, exprSources: _*)
				(Constant(TVoid, 0), activeBlock)
			case TFunction(_, returnType) =>
				val resultSource = Register(returnType, s"%${function.nameGenerator.nextRegister}")
				activeBlock += Call(resultSource, functionPtr, exprSources: _*)
				if returnType == TBool then
					considerJumping(resultSource, activeBlock)
				else
					(resultSource, activeBlock)
		}
	}

	override def visitEParen(ctx: LatteParser.EParenContext): (DefinedValue, Block) = visit(ctx.expr)
}


/**
 * Assembles values in non-SSA form.
 * This visitor returns the Source of the pointer to the requested value, or a pointer for write access.
 * The second value in the returned tuple signifies whether a source with the pointer
 * to the writable value was returned instead of the value itself.
 * @param symbolStack    The symbol stack to use when assembling an expression.
 * @param classTable     Information about classes defined in the program.
 * @param thisBlock      The block which is being assembled.
 * @param hostClass      The possible host class of the function being assembled.
 */
class ValueAssembler(
	using
	symbolStack: SymbolStack[SymbolSourceInfo],
	classTable : ClassTable,
	function   : Function,
	thisBlock  : Block,
	hostClass  : Option[String]
) extends LatteBaseVisitor[(Source, Boolean, Block)] {
	override def visitVSelf(ctx: LatteParser.VSelfContext): (Register, Boolean, Block) =
		(Register(TClass(hostClass.get), NamingConvention.self), false, thisBlock)

	override def visitVID(ctx: LatteParser.VIDContext): (Source, Boolean, Block) =
		(symbolStack.getOrThrow(ctx.ID.getText, Position.fromToken(ctx.ID.getSymbol)).source, false, thisBlock)

	/**
	 * Visits a value and if a pointer to a writable value was returned, then it loads the pointer.
	 * @param ctx The value to visit
	 * @return The read-only value.
	 */
	def visitForAccess(ctx: ParseTree): (Source, Block) = {
		val (sourceMaybePtr, writable, activeBlock) = visit(ctx)
		if writable then
			val underlyingType = sourceMaybePtr.valueType.asInstanceOf[PointerTo].underlying
			val sourceRegister = Register(underlyingType, s"%${function.nameGenerator.nextRegister}")
			activeBlock += Bitcast(sourceRegister, sourceMaybePtr.asInstanceOf[Register])
			(sourceRegister, activeBlock)
		else
			(sourceMaybePtr, activeBlock)
	}

	override def visitVMem(ctx: LatteParser.VMemContext): (Register, Boolean, Block) = {
		// Get the source of the object owning the member.
		val (objectSource, activeBlock) = visitForAccess(ctx.value).asInstanceOf[(Register, Block)]

		// Get the source of the member.
		val TClass(className) = objectSource.valueType.asInstanceOf[TClass]
		// TODO: array.length
		val MemberInfo(_, _, memberType, _, offset) = classTable(className).memberTable.apply(ctx.ID.getText)
		memberType match {
			case functionType: TFunction =>
				// If it's a function, it cannot be written to, so return function pointer.
				val objectSourcePtr = Register(PointerTo(PointerTo(FunctionPointer)), s"%${function.nameGenerator.nextRegister}")
				activeBlock += Bitcast(objectSourcePtr, objectSource)
				val vTablePtr = Register(PointerTo(FunctionPointer), s"%${function.nameGenerator.nextRegister}")
				activeBlock += PtrLoad(vTablePtr, objectSourcePtr)
				val functionPtrPtr = Register(PointerTo(FunctionPointer), s"%${function.nameGenerator.nextRegister}")
				activeBlock += GetElementPtr(functionPtrPtr, vTablePtr, Constant(TInt, offset))
				val functionPtr = Register(FunctionPointer, s"%${function.nameGenerator.nextRegister}")
				activeBlock += PtrLoad(functionPtr, functionPtrPtr)
				val functionPtrTyped = Register(memberType, s"%${function.nameGenerator.nextRegister}")
				activeBlock += Bitcast(functionPtrTyped, functionPtr)
				(functionPtrTyped, false, activeBlock)
			case nonFunctionType =>
				// A field can be written to, so return pointer to it.
				val memberPointer = Register(PointerTo(memberType), s"%${function.nameGenerator.nextRegister}")
				activeBlock += GetElementPtr(memberPointer, objectSource, Constant(TInt, 0), Constant(TInt, offset))
				(memberPointer, true, activeBlock)
		}
	}

	override def visitVArr(ctx: LatteParser.VArrContext): (Register, Boolean, Block) = {
		// Get the source of the array containing the member.
		val (arraySource, activeBlockA) = visitForAccess(ctx.value).asInstanceOf[(Register, Block)]

		// Get the source of the index.
		val (indexSource, activeBlock) = ExpressionAssembler()(using thisBlock = activeBlockA, blocksAfter = None).visit(ctx.expr)

		// Get the source of the element.
		val underlyingType = arraySource.valueType.asInstanceOf[TArray].underlying
		val resultSource = Register(PointerTo(underlyingType), s"%${function.nameGenerator.nextRegister}")
		activeBlock += GetElementPtr(resultSource, arraySource, indexSource)
		(resultSource, true, activeBlock)
	}
}
