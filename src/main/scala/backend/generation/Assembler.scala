package backend.generation

import backend.generation.StatementAssembler.{decreaseReferenceCounts, increaseReferenceCounts}
import backend.generation.ValueAssembler.assembleArrayAccess
import backend.representation.*
import frontend.checks.symbols.{ClassTable, MemberInfo, SymbolStack}
import frontend.checks.types.{CompilerType, LatteType, TypeCollector}
import frontend.checks.types.LatteType.*
import frontend.Position
import frontend.checks.types.CompilerType.{CTAnyPointer, CTFunction, CTFunctionPointer, CTPointerTo}
import grammar.{LatteBaseVisitor, LatteParser}
import org.antlr.v4.runtime.tree.ParseTree

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

case class GenerationError(message: String) extends Error

/**
 * Assembles statements in non-SSA form.
 * @param symbolStack     The symbol stack to use when assembling a statement.
 * @param classTable      Information about classes defined in the program.
 * @param function        The function which is being assembled.
 * @param thisBlock       The block which is being assembled.
 * @param hostClass       The possible host class of the function being assembled.
 * @param blockAfter      The possible block which should be jumped to at the end of [[thisBlock]].
 */
class StatementAssembler()(
	using
	symbolStack    : SymbolStack[SymbolSourceInfo],
	classTable     : ClassTable,
	function       : Function,
	thisBlock      : Block,
	hostClass      : Option[String],
	blockAfter     : Option[Block]
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

	// Decrease reference counts for every variable declared in a function.
	// This searches through the symbol stack and clears all variables until it
	// reaches the frame where the function and its arguments were defined.
	private def decreaseReferenceCountsInFunction(block: Block): Unit = {
		symbolStack.tables.foldLeft(false) { (skip, symbolTable) =>
			if skip then
				skip
			else
				val sourceInfos = symbolTable.values.toSeq
				decreaseReferenceCounts(
					sourceInfos.collect { case info if !Seq(NamingConvention.self, function.name).contains(info.symbolName) => info.source},
					function,
					block
				)
				sourceInfos.exists(_.symbolName == function.name)
		}
	}

	override def visitSEmpty(ctx: LatteParser.SEmptyContext): (Block, Boolean) = (thisBlock, false)

	override def visitBlock(ctx: LatteParser.BlockContext): (Block, Boolean) = {
		symbolStack.addScope()

		// Assemble the statements
		val stmts = ctx.stmt.asScala.toSeq
		val unboundPointersInBlock: mutable.Set[Register] = mutable.Set.empty
		val (activeBlock, brokeFlow) = stmts.foldLeft((thisBlock, false)) {
			case ((block, brokeFlow), stmt) =>
				if brokeFlow then
					(block, brokeFlow)
				else
					StatementAssembler()(using thisBlock = block, blockAfter = None).visit(stmt)
		}

		// If the block does not break flow with a return,
		// release references bound to variables declared in block.
		if !brokeFlow then
			decreaseReferenceCounts(
				symbolStack.tables.head.values.map(_.source),
				function,
				activeBlock
			)
			activeBlock += CallVoid(Label(CTFunction(Seq(), TVoid), "@clearUnboundPointers"))

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
		val (valueSource, writable, _, activeBlockV) = ValueAssembler().visit(ctx.value).asInstanceOf[(DefinedValue, Boolean, Option[Register], Block)]
		val valueType = valueSource.valueType
		val (exprSource, activeBlock) = ExpressionAssembler()(using blocksAfter = None).visit(ctx.expr)
		val exprType = exprSource.valueType

		def castIfNecessary(targetType: CompilerType, value: DefinedValue, block: Block): DefinedValue = {
			if targetType == exprType then
				exprSource
			else
				val castResult = Register(targetType, s"%${function.nameGenerator.nextRegister}")
				block += Bitcast(castResult, exprSource)
				castResult
		}

		// If it is writable, then we have a pointer.
		// If it is not writable, then we must be dealing with a local variable or void.
		// Furthermore, we must perform a bitcast if the types do not match,
		// because that means we are dealing with an object of a subclass.
		if writable then {
			val dereferencedValueType = valueType.asInstanceOf[CTPointerTo].underlying
			val exprSourceAfterCast = castIfNecessary(dereferencedValueType, exprSource, activeBlock)
			val vanishingReference = Register(dereferencedValueType, s"%${function.nameGenerator.nextRegister}")
			activeBlock += PtrLoad(vanishingReference, valueSource.asInstanceOf[Register])
			increaseReferenceCounts(Seq(exprSourceAfterCast), function, activeBlock)
			decreaseReferenceCounts(Seq(vanishingReference), function, activeBlock)
			activeBlock += PtrStore(valueSource.asInstanceOf[Register], exprSourceAfterCast)
		} else {
			if valueSource.valueType != TVoid then
				val exprSourceAfterCast = castIfNecessary(valueType, exprSource, activeBlock)
				increaseReferenceCounts(Seq(exprSourceAfterCast), function, activeBlock)
				decreaseReferenceCounts(Seq(valueSource), function, activeBlock)
				activeBlock += Copy(valueSource.asInstanceOf[Register], exprSourceAfterCast)
		}
		considerJumping(activeBlock, false)
	}

	private def visitPostcrement(ctx: LatteParser.SIncrContext | LatteParser.SDecrContext): (Block, Boolean) = {
		val ((valueSource, writable, _, activeBlock), op) = ctx match {
			case incr: LatteParser.SIncrContext => (ValueAssembler().visit(incr.value).asInstanceOf[(Register, Boolean, Option[Register], Block)], Plus)
			case decr: LatteParser.SDecrContext => (ValueAssembler().visit(decr.value).asInstanceOf[(Register, Boolean, Option[Register], Block)], Minus)
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

		// Before returning, we must decrease reference counts
		// for all variables declared in the function.
		decreaseReferenceCountsInFunction(thisBlock)
		// Increase reference count for returned result to avoid clearing it.
		// Then, clear unbound pointers, then decrease the reference count again.
		increaseReferenceCounts(Seq(resultSource), function, activeBlock)
		activeBlock += CallVoid(Label(CTFunction(Seq(), TVoid), "@clearUnboundPointers"))
		decreaseReferenceCounts(Seq(resultSource), function, activeBlock)

		if resultSource.valueType == TVoid then
			activeBlock += ReturnVoid
		else
			activeBlock += Return(resultSource)
		(activeBlock, true)
	}

	override def visitSRetVoid(ctx: LatteParser.SRetVoidContext): (Block, Boolean) = {
		// Before returning, we must decrease reference counts
		// for all variables declared in the function.
		decreaseReferenceCountsInFunction(thisBlock)
		thisBlock += CallVoid(Label(CTFunction(Seq(), TVoid), "@clearUnboundPointers"))

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
		// Construct the preamble, which is taking the source of the array and its length.
		val iteratorName: String = ctx.ID.getText
		val iteratorType: LatteType = TypeCollector(using classTable.keys.toSet).visit(ctx.basicType)
		val (arraySource, activeBlock, _) = ValueAssembler().visitForRead(ctx.expr).asInstanceOf[(Register, Block, Option[Register])]
		val arrayLengthSource = ValueAssembler.assembleArrayLength(arraySource, activeBlock)

		val blockCond = function.addBlock()
		val blockStmt = function.addBlock()
		val blockAfterLoop = function.addBlock()

		activeBlock += Jump(blockCond.name)
		function.addJump(activeBlock.name, blockCond.name)

		// Construct the index registers.
		// Wait for the statement block to be ready before constructing condition block in order to determine phi arguments.
		val indexRegister = Register(TInt, s"%${function.nameGenerator.nextRegister}")
		val nextIndexRegister = Register(TInt, s"%${function.nameGenerator.nextRegister}")

		// Construct the loop body block.
		val iteratorValue = ValueAssembler.assembleArrayRead(arraySource, indexRegister, blockStmt)
		symbolStack.addScope(mutable.HashMap(iteratorName -> SymbolSourceInfo(iteratorName, None, iteratorValue)))
		val (blockStmtLast, brokeFlow) = StatementAssembler()(using thisBlock = blockStmt, blockAfter = None).visit(ctx.stmt)
		symbolStack.removeScope()
		if !brokeFlow then {
			blockStmtLast += Jump(blockCond.name)
			function.addJump(blockStmtLast.name, blockCond.name)
		}

		// Now we can construct a phi instruction for the index register.
		blockCond += (if !brokeFlow then
			Phi(indexRegister, PhiCase(activeBlock.name, Constant(TInt, 0)), PhiCase(blockStmtLast.name, nextIndexRegister))
		else
			Copy(indexRegister, Constant(TInt, 0))
		)
		blockCond += BinOp(nextIndexRegister, indexRegister, Plus, Constant(TInt, 1))
		val comparisonRegister = Register(TBool, s"%${function.nameGenerator.nextRegister}")
		blockCond += BinOp(comparisonRegister, indexRegister, Lt, arrayLengthSource)
		blockCond += ConditionalJump(comparisonRegister, blockStmt.name, blockAfterLoop.name)
		function.addJump(blockCond.name, blockStmt.name)
		function.addJump(blockCond.name, blockAfterLoop.name)

		// Phew
		considerJumping(blockAfterLoop, brokeFlow)
	}

	override def visitSExp(ctx: LatteParser.SExpContext): (Block, Boolean) = {
		considerJumping(
			ExpressionAssembler()(using blocksAfter = blockAfter.map { b => (b, b) }).visit(ctx.expr)._2,
			false
		)
	}
}

object StatementAssembler {
	private def changeReferenceCounts(sources: Iterable[Source], function: Function, block: Block, decrease: Boolean): Unit = {
		sources.foreach { source =>
			if source.valueType.isInstanceOf[TStr.type | TArray | TClass] then
				val pointerRegister = Register(CTAnyPointer, s"%${function.nameGenerator.nextRegister}")
				block += Bitcast(pointerRegister, source.asInstanceOf[DefinedValue])
				block += CallVoid(Label(CTFunction(Seq(CTAnyPointer), TVoid), if decrease then "@decreaseRefCount" else "@increaseRefCount"), pointerRegister)
		}
	}

	def decreaseReferenceCounts(sources: Iterable[Source], function: Function, block: Block): Unit =
		changeReferenceCounts(sources, function, block, true)

	def increaseReferenceCounts(sources: Iterable[Source], function: Function, block: Block): Unit =
		changeReferenceCounts(sources, function, block, false)
}

/**
 * Assembles items in non-SSA form.
 *
 * @param symbolStack     The symbol stack to use when assembling a statement.
 * @param classTable      Information about classes defined in the program.
 * @param function        The function which is being assembled.
 * @param thisBlock       The block which is being assembled.
 * @param hostClass       The possible host class of the function being assembled.
 */
class ItemAssembler(
	itemType   : LatteType
)(
	using
	symbolStack    : SymbolStack[SymbolSourceInfo],
	classTable     : ClassTable,
	function       : Function,
	thisBlock      : Block,
	hostClass      : Option[String]
) extends LatteBaseVisitor[Block] {
	override def visitItem(ctx: LatteParser.ItemContext): Block = {
		val expr = ctx.expr
		val itemName = ctx.ID.getText
		if expr == null then
			val resultRegister = Register(itemType, s"%${function.nameGenerator.nextRegister}")
			thisBlock += Copy(resultRegister, Constant(itemType, 0))
			symbolStack.add(SymbolSourceInfo(itemName, None, resultRegister))
			thisBlock
		else
			val (itemSource, activeBlock) = ExpressionAssembler()(using blocksAfter = None).visit(expr)

			val resultRegister = Register(itemSource.valueType, s"%${function.nameGenerator.nextRegister}")
			activeBlock += Copy(resultRegister, itemSource)

			increaseReferenceCounts(Seq(resultRegister), function, activeBlock)

			symbolStack.add(SymbolSourceInfo(itemName, None, resultRegister))
			activeBlock
	}
}

/**
 * Assembles expressions in non-SSA form.
 * This visitor returns the Source of the result of the expression, and the active block where the source is to be used.
 * @param symbolStack     The symbol stack to use when assembling an expression.
 * @param classTable      Information about classes defined in the program.
 * @param function        The function which is being assembled.
 * @param thisBlock       The block which is being assembled.
 * @param hostClass       The possible host class of the function being assembled.
 * @param blocksAfter     The possible blocks which should be jumped to after computing the expression in the form of (ifTrue, ifFalse).
 */
class ExpressionAssembler()(
	using
	symbolStack    : SymbolStack[SymbolSourceInfo],
	classTable     : ClassTable,
	function       : Function,
	thisBlock      : Block,
	hostClass      : Option[String],
	blocksAfter    : Option[(Block, Block)]
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

	// If an expression created a new entity in memory (string, array, or object),
	// we must register it immediately for memory management.
	private def registerEntity(pointerSource: Register, thisBlock: Block): Unit = pointerSource.valueType match {
		case TStr =>
			val pointerRegister = Register(CTAnyPointer, s"%${function.nameGenerator.nextRegister}")
			thisBlock += Bitcast(pointerRegister, pointerSource)
			thisBlock += CallVoid(Label(CTFunction(Seq(CTAnyPointer), TVoid), "@registerString"), pointerRegister)
		case TArray(underlying) =>
			val pointerRegister = Register(CTAnyPointer, s"%${function.nameGenerator.nextRegister}")
			val containsPointers = Constant(TBool, if underlying.isInstanceOf[TStr.type | TArray | TClass] then 1 else 0)
			thisBlock += Bitcast(pointerRegister, pointerSource)
			thisBlock += CallVoid(Label(CTFunction(Seq(CTAnyPointer, TBool), TVoid), "@registerArray"), pointerRegister, containsPointers)
		case TClass(className) =>
			val pointerRegister = Register(CTAnyPointer, s"%${function.nameGenerator.nextRegister}")
			val classID = Constant(TInt, classTable(className).classID)
			thisBlock += Bitcast(pointerRegister, pointerSource)
			thisBlock += CallVoid(Label(CTFunction(Seq(CTAnyPointer, TInt), TVoid), "@registerObject"), pointerRegister, classID)
		case _ => throw GenerationError("Unexpected entity type registered for memory management.")
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
				registerEntity(resultSource, thisBlock)
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
				if l.valueType == TStr then
					thisBlock += Call(resultRegister, Label(TFunction(Seq(TStr, TStr), TBool), "@compareStrings"), l, r)
				else
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
				function.addJump(blockIfTrue.name, activeBlock.name)
				blockIfFalse += Jump(activeBlock.name)
				function.addJump(blockIfFalse.name, activeBlock.name)
				val resultSource = Register(TBool, s"%${function.nameGenerator.nextRegister}")
				activeBlock += Phi(resultSource, PhiCase(blockIfTrue.name, Constant(TBool, 1)), PhiCase(blockIfFalse.name, Constant(TBool, 0)))
				(resultSource, activeBlock)
		}
	}

	override def visitEVal(ctx: LatteParser.EValContext): (DefinedValue, Block) = {
		val (resultSource, activeBlock, _) = ValueAssembler().visitForRead(ctx).asInstanceOf[(DefinedValue, Block, Option[Register])]
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
		val sizePtr = Register(TClass(className), s"%${function.nameGenerator.nextRegister}")
		thisBlock += GetElementPtr(sizePtr, Constant(TClass(className), 0), Constant(TInt, 1))
		val sizeInt = Register(TInt, s"%${function.nameGenerator.nextRegister}")
		thisBlock += PtrToInt(sizeInt, sizePtr)
		val allocSource = Register(CTAnyPointer, s"%${function.nameGenerator.nextRegister}")
		thisBlock += Call(allocSource, Label(CTFunction(Seq(TInt, TInt), CTAnyPointer), "@calloc"), Constant(TInt, 1), sizeInt)
		val objectPtr = Register(TClass(className), s"%${function.nameGenerator.nextRegister}")
		thisBlock += Bitcast(objectPtr, allocSource)
		thisBlock += CallVoid(Label(TFunction(Seq(TClass(className)), TVoid), NamingConvention.constructor(className)), objectPtr)
		registerEntity(objectPtr, thisBlock)
		(objectPtr, thisBlock)
	}

	override def visitENewArr(ctx: LatteParser.ENewArrContext): (DefinedValue, Block) = {
		// Collect info about the new array.
		val (arraySizeValue, activeBlock) = visit(ctx.expr)
		val underlyingType = TypeCollector(using classTable.keys.toSet).visit(ctx.basicType).asInstanceOf[TNonFun]

		val sizeToCalloc = underlyingType match {
			case TVoid =>
				// Only 8 bytes for the length.
				Constant(TInt, 8)
			case _ =>
				// Each element of an array takes up 8 bytes.
				val r1 = Register(TInt, s"%${function.nameGenerator.nextRegister}")
				activeBlock += BinOp(r1, arraySizeValue, Mul, Constant(TInt, 8))
				// Add 8 bytes for the length.
				val r2 = Register(TInt, s"%${function.nameGenerator.nextRegister}")
				activeBlock += BinOp(r2, r1, Plus, Constant(TInt, 8))
				r2
		}
		val allocSource = Register(CTAnyPointer, s"%${function.nameGenerator.nextRegister}")
		activeBlock += Call(allocSource, Label(CTFunction(Seq(TInt, TInt), CTAnyPointer), "@calloc"), sizeToCalloc, Constant(TInt, 8))

		// Initiate the array size, offset the array pointer and return it.
		val arraySizePtr = Register(CTPointerTo(TInt), s"%${function.nameGenerator.nextRegister}")
		activeBlock += Bitcast(arraySizePtr, allocSource)
		activeBlock += PtrStore(arraySizePtr, arraySizeValue)
		val ptrValueToReturn = Register(CTAnyPointer, s"%${function.nameGenerator.nextRegister}")
		activeBlock += GetElementPtr(ptrValueToReturn, allocSource, Constant(TInt, 8))
		val ptrRegisterToReturn = Register(TArray(underlyingType), s"%${function.nameGenerator.nextRegister}")
		activeBlock += Bitcast(ptrRegisterToReturn, ptrValueToReturn)
		registerEntity(ptrRegisterToReturn, activeBlock)
		(ptrRegisterToReturn, activeBlock)
	}

	override def visitENull(ctx: LatteParser.ENullContext): (DefinedValue, Block) = {
		val className = ctx.ID.getText
		(Constant(TClass(className), 0), thisBlock)
	}

	override def visitEFunCall(ctx: LatteParser.EFunCallContext): (DefinedValue, Block) = {
		val (functionPtr, activeBlockF, hostObjectSource) = ValueAssembler().visitForRead(ctx.value).asInstanceOf[(Name, Block, Option[Register])]
		val functionType = functionPtr.valueType.asInstanceOf[TFunction]
		val argTypes = functionType.args.filterNot(_ == TVoid)
		var activeBlock = activeBlockF
		val exprs: Seq[LatteParser.ExprContext] = if ctx.expr == null then Seq.empty[LatteParser.ExprContext] else ctx.expr.asScala.toSeq

		val exprSources: Seq[DefinedValue] = exprs.map { expr =>
			val (exprSource, activeBlockE) = ExpressionAssembler()(using blocksAfter = None).visit(expr)
			activeBlock = activeBlockE
			exprSource
		}.filterNot(_.valueType == TVoid).prependedAll(hostObjectSource)

		val exprSourcesAfterCasts = argTypes.zip(exprSources).map { (argType, exprSource) =>
			if argType != exprSource.valueType then
				// We must be dealing with an object subtype.
				val exprSourceAfterCast = Register(argType, s"%${function.nameGenerator.nextRegister}")
				activeBlock += Bitcast(exprSourceAfterCast, exprSource)
				exprSourceAfterCast
			else
				exprSource
		}

		functionPtr.valueType.asInstanceOf[TFunction] match {
			case TFunction(_, TVoid) =>
				activeBlock += CallVoid(functionPtr, exprSourcesAfterCasts: _*)
				(Constant(TVoid, 0), activeBlock)
			case TFunction(argTypes, returnType) =>
				val resultSource = Register(returnType, s"%${function.nameGenerator.nextRegister}")
				activeBlock += Call(resultSource, functionPtr, exprSourcesAfterCasts: _*)
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
 * This visitor returns the Source of the requested value, or a pointer for write access.
 * The second value in the returned tuple signifies whether a source with the pointer
 * to the writable value was returned instead of the value itself.
 * The third return value is the pointer to a possible host object.
 * @param symbolStack     The symbol stack to use when assembling an expression.
 * @param classTable      Information about classes defined in the program.
 * @param thisBlock       The block which is being assembled.
 * @param hostClass       The possible host class of the function being assembled.
 */
class ValueAssembler(
	using
	symbolStack    : SymbolStack[SymbolSourceInfo],
	classTable     : ClassTable,
	function       : Function,
	thisBlock      : Block,
	hostClass      : Option[String]
) extends LatteBaseVisitor[(Source, Boolean, Option[Register], Block)] {
	override def visitVSelf(ctx: LatteParser.VSelfContext): (Register, Boolean, Option[Register], Block) =
		(Register(TClass(hostClass.get), NamingConvention.self), false, None, thisBlock)

	override def visitVID(ctx: LatteParser.VIDContext): (Source, Boolean, Option[Register], Block) = {
		val symbolSourceInfoFromSymTable = symbolStack.getOrThrow(ctx.ID.getText, Position.fromToken(ctx.ID.getSymbol))

		symbolSourceInfoFromSymTable.hostClass match {
			case None =>
				// We're dealing with a variable.
				(symbolSourceInfoFromSymTable.source, false, None, thisBlock)
			case Some(className) =>
				// We're dealing with a class member
				getMember(Register(TClass(className), NamingConvention.self), className, ctx.ID.getText, thisBlock)
		}
	}

	/**
	 * Visits a value and if a pointer to a writable value was returned, then it loads the pointer.
	 * @param ctx The value to visit
	 * @return The read-only value.
	 */
	def visitForRead(ctx: ParseTree): (Source, Block, Option[Register]) = {
		val (sourceMaybePtr, writable, hostObjectSource, activeBlock) = visit(ctx)
		if writable then
			val underlyingType = sourceMaybePtr.valueType.asInstanceOf[CTPointerTo].underlying
			val sourceRegister = Register(underlyingType, s"%${function.nameGenerator.nextRegister}")
			activeBlock += PtrLoad(sourceRegister, sourceMaybePtr.asInstanceOf[Register])
			(sourceRegister, activeBlock, hostObjectSource)
		else
			(sourceMaybePtr, activeBlock, hostObjectSource)
	}

	override def visitVMem(ctx: LatteParser.VMemContext): (Source, Boolean, Option[Register], Block) = {
		// Get the source of the object owning the member.
		val (entitySource, activeBlock, _) = visitForRead(ctx.value).asInstanceOf[(Register, Block, Option[Register])]

		entitySource.valueType match {
			case TClass(className) =>
				// Get the source of the member.
				getMember(entitySource, className, ctx.ID.getText, activeBlock)
			case TArray(underlying) =>
				// The only member of an array is its length.
				val arrayLengthSource = ValueAssembler.assembleArrayLength(entitySource, activeBlock)
				(arrayLengthSource, false, None, activeBlock)
			case _ => throw GenerationError("Unexpected host entity of member value.")
		}
	}

	private def getMember(classSource: Register, className: String, memberName: String, activeBlock: Block): (Source, Boolean, Option[Register], Block) = {
		val MemberInfo(_, _, memberType, _, offset) = classTable(className).memberTable.apply(memberName)
		memberType match {
			case functionType: TFunction =>
				val functionTypeWithSelf = functionType.copy(args = functionType.args.prepended(TClass(className)))
				// If it's a function, it cannot be written to, so return function pointer.
				val objectSourcePtr = Register(CTPointerTo(CTPointerTo(CTFunctionPointer)), s"%${function.nameGenerator.nextRegister}")
				activeBlock += Bitcast(objectSourcePtr, classSource)
				val vTablePtr = Register(CTPointerTo(CTFunctionPointer), s"%${function.nameGenerator.nextRegister}")
				activeBlock += PtrLoad(vTablePtr, objectSourcePtr)
				val functionPtrPtr = Register(CTPointerTo(CTFunctionPointer), s"%${function.nameGenerator.nextRegister}")
				activeBlock += GetElementPtr(functionPtrPtr, vTablePtr, Constant(TInt, offset))
				val functionPtr = Register(CTFunctionPointer, s"%${function.nameGenerator.nextRegister}")
				activeBlock += PtrLoad(functionPtr, functionPtrPtr)
				val functionPtrTyped = Register(functionTypeWithSelf, s"%${function.nameGenerator.nextRegister}")
				activeBlock += Bitcast(functionPtrTyped, functionPtr)
				(functionPtrTyped, false, Some(classSource), activeBlock)
			case TVoid =>
				(Constant(TVoid, 0), false, Some(classSource), activeBlock)
			case nonFunctionType =>
				// A field can be written to, so return pointer to it.
				val memberPointer = Register(CTPointerTo(memberType), s"%${function.nameGenerator.nextRegister}")
				activeBlock += GetElementPtr(memberPointer, classSource, Constant(TInt, 0), Constant(TInt, offset))
				(memberPointer, true, Some(classSource), activeBlock)
		}
	}

	override def visitVArr(ctx: LatteParser.VArrContext): (Source, Boolean, Option[Register], Block) = {
		// Get the source of the array containing the member.
		val (arraySource, activeBlockA, _) = visitForRead(ctx.value).asInstanceOf[(Register, Block, Option[Register])]
		// Get the source of the index.
		val (indexSource, activeBlock) = ExpressionAssembler()(using thisBlock = activeBlockA, blocksAfter = None).visit(ctx.expr)
		// Get the source of the element.
		// canWrite is false only for void.
		val (resultSource, canWrite) = ValueAssembler.assembleArrayAccess(arraySource, indexSource, activeBlock)
		(resultSource, canWrite, None, activeBlock)
	}
}

object ValueAssembler {
	def assembleArrayAccess(arraySource: Register, indexSource: DefinedValue, block: Block)(using function: Function): (DefinedValue, Boolean) = {
		val underlyingType = arraySource.valueType.asInstanceOf[TArray].underlying
		underlyingType match {
			case TVoid =>
				(Constant(TVoid, 0), false)
			case _ =>
				val resultSource = Register(CTPointerTo(underlyingType), s"%${function.nameGenerator.nextRegister}")
				block += GetElementPtr(resultSource, arraySource, indexSource)
				(resultSource, true)
		}
	}

	def assembleArrayRead(arraySource: Register, indexSource: DefinedValue, block: Block)(using function: Function): DefinedValue = {
		val underlyingType = arraySource.valueType.asInstanceOf[TArray].underlying
		underlyingType match {
			case TVoid =>
				Constant(TVoid, 0)
			case _ =>
				// As long as the underlying type is not Void,
				// we receive a pointer to the element and write access.
				val (elementPtr, _) = assembleArrayAccess(arraySource, indexSource, block).asInstanceOf[(Register, Boolean)]
				val resultSource = Register(underlyingType, s"%${function.nameGenerator.nextRegister}")
				block += PtrLoad(resultSource, elementPtr)
				resultSource
		}
	}

	def assembleArrayLength(arraySource: Register, block: Block)(using function: Function): Register = {
		val arrayPtrAsIntPtr = Register(CTPointerTo(TInt), s"%${function.nameGenerator.nextRegister}")
		block += Bitcast(arrayPtrAsIntPtr, arraySource)
		val arrayLengthPtr = Register(CTPointerTo(TInt), s"%${function.nameGenerator.nextRegister}")
		block += GetElementPtr(arrayLengthPtr, arrayPtrAsIntPtr, Constant(TInt, -1))
		val arrayLengthSource = Register(TInt, s"%${function.nameGenerator.nextRegister}")
		block += PtrLoad(arrayLengthSource, arrayLengthPtr)
		arrayLengthSource
	}
}
