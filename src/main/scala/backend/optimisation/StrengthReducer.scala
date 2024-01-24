package backend.optimisation

import backend.representation.{Assignment, BinOp, BinaryOperator, Constant, Copy, Function, Instruction, Minus, Mul, Phi, PhiCase, Plus, Register}
import frontend.checks.types.LatteType.TInt

object StrengthReducer {
  def optimiseFunction(using function: Function): Boolean = {
    var somethingChanged = false
    var stop = false
    while !stop do {
      stop = !reduceStrength
      if !stop then somethingChanged = true
    }
    somethingChanged
  }

  private def reduceStrength(using function: Function): Boolean = {
    var somethingChanged = false
    val inductionVariables = findInductionVariables
    for ((inductionRegister, (phiPos, binOpPos)) <- inductionVariables if !somethingChanged) do {
      // Collect information.
      val phi: Phi = function.blocks(phiPos._1).instructions(phiPos._2).asInstanceOf[Phi]
      val binOp: BinOp = function.blocks(binOpPos._1).instructions(binOpPos._2).asInstanceOf[BinOp]
      val (start, step) = getStartAndStep(phi, binOp)
      val reductionOpportunitiesBeforeStep = getReductionOpportunities(phi.dst).filter(_._1 != binOp.dst)
      val reductionOpportunitiesAfterStep = getReductionOpportunities(binOp.dst)

      val (phiBlockDefault, phiBlockStep) = phi.cases.head.value match {
        case c: Constant => (phi.cases.head.blockName, phi.cases(1).blockName)
        case _ => (phi.cases(1).blockName, phi.cases.head.blockName)
      }

      // Generate new instructions.
      val newInstructions: Map[Register, (Phi, BinOp)] =
        reductionOpportunitiesBeforeStep.map { case (dst, (op, value, flipped)) =>
          dst -> reduceBinOp(start, step, op, value, flipped, phiBlockDefault, phiBlockStep)
        } ++
        reductionOpportunitiesAfterStep.map { case (dst, (op, value, flipped)) =>
          dst -> reduceBinOp(start + step, step, op, value, flipped, phiBlockDefault, phiBlockStep)
        }

      val newPhis: Seq[Phi] = newInstructions.values.toSeq.map(_._1)
      val newBinOps: Seq[BinOp] = newInstructions.values.toSeq.map(_._2)

      // Inject new instructions.
      if newInstructions.nonEmpty then
        somethingChanged = true
        function.blocks(phiPos._1).instructions.insertAll(phiPos._2 + 1, newPhis)
        function.blocks(binOpPos._1).instructions.insertAll(binOpPos._2 + 1, newBinOps)

        newInstructions.foreach { case (oldDst, (Phi(newDst, _ : _*), _)) =>
          substitute(Copy(oldDst, newDst))
        }
        eliminateNullInstructions
    }
    somethingChanged
  }

  private def substitute(copy: Copy)(using function: Function): Unit = {
    for (b <- function.nonNullBlocks; iIdx <- b.instructions.indices) {
      if b.instructions(iIdx) != null then
        b.instructions(iIdx) match {
          case a: Assignment if a.dst == copy.dst => b.instructions(iIdx) = null
          case i => b.instructions(iIdx) = i.substitute(copy.dst, copy.value)
        }
    }
  }

  private def eliminateNullInstructions(using function: Function): Unit = {
    function.nonNullBlocks.foreach(_.removeNulls())
  }

  private def reduceBinOp(
    indStart: Long, indStep: Long, op: BinaryOperator, value: Long, flipped: Boolean, phiBlockDefault: String, phiBlockStep: String
  )(
    using function: Function
  ): (Phi, BinOp) = {
    val newRegisterPhi = Register(TInt, function.nameGenerator.nextRegister)
    val newRegisterBinOp = Register(TInt, function.nameGenerator.nextRegister)

    op match {
      case Plus => (
        Phi(newRegisterPhi, PhiCase(phiBlockDefault, Constant(TInt, indStart + value)), PhiCase(phiBlockStep, newRegisterBinOp)),
        BinOp(newRegisterBinOp, newRegisterPhi, Plus, Constant(TInt, indStep))
      )
      case Minus => (
        Phi(newRegisterPhi, PhiCase(phiBlockDefault, Constant(TInt, if !flipped then indStart - value else value - indStart)), PhiCase(phiBlockStep, newRegisterBinOp)),
        BinOp(newRegisterBinOp, newRegisterPhi, Plus, Constant(TInt, if !flipped then indStep else -indStep))
      )
      case Mul => (
        Phi(newRegisterPhi, PhiCase(phiBlockDefault, Constant(TInt, indStart * value)), PhiCase(phiBlockStep, newRegisterBinOp)),
        BinOp(newRegisterBinOp, newRegisterPhi, Plus, Constant(TInt, indStep * value))
      )
      case _ => throw RuntimeException(s"Unexpected operator in strength reduction: $op.")
    }
  }

  // Returns a mapping from the register to remove to the operator, the value, and whether the induction register comes second.
  private def getReductionOpportunities(ind: Register)(using function: Function): Map[Register, (BinaryOperator, Long, Boolean)] = {
    (for (bIdx <- function.nonNullBlockIndices; iIdx <- function.blocks(bIdx).instructions.indices) yield {
      function.blocks(bIdx).instructions(iIdx)
    }).collect {
      case BinOp(dst, arg1, op, arg2)
        if dst != ind && (op == Plus || op == Minus || op == Mul)
          && ((arg1 == ind && arg2.isInstanceOf[Constant]) || (arg2 == ind && arg1.isInstanceOf[Constant])) =>
        dst -> (op, if arg1 == ind then arg2.asInstanceOf[Constant].value else arg1.asInstanceOf[Constant].value, ind == arg2)
    }.toMap
  }

  // The induction variable starts at `start` and *increases* by `step`.
  private def getStartAndStep(phi: Phi, binOp: BinOp): (Long, Long) = {
    val start: Long = phi.cases.head.value match {
      case Constant(_, v) => v
      case _ => phi.cases(1).value.asInstanceOf[Constant].value
    }
    val step: Long = if binOp.arg1 == phi.dst then binOp.arg2.asInstanceOf[Constant].value else binOp.arg1.asInstanceOf[Constant].value
    (start, if binOp.op == Minus then -step else step)
  }

  // Find all registers such that
  // 1. they are assigned to with Phi with two cases, one constant and one register
  // 2. the register in the phi case is obtained by taking the first register
  //    and adding a constant or multiplying by a constant.
  // Return a map with the interesting registers and the positions of the aforementioned instructions.
  private def findInductionVariables(using function: Function): Map[Register, ((Int, Int), (Int, Int))] = {
    // We take all interesting Phis, additions and multiplications.
    val interestingInstructions: Seq[(Assignment, (Int, Int))] =
      (for(bIdx <- function.nonNullBlockIndices; iIdx <- function.blocks(bIdx).instructions.indices) yield {
        (function.blocks(bIdx).instructions(iIdx), (bIdx, iIdx))
      }).collect {
        case (phi @ Phi(_, PhiCase(_, case1), PhiCase(_, case2)), pos)
          if (case1.isInstanceOf[Constant] && case2.isInstanceOf[Register])
            || (case1.isInstanceOf[Register] && case2.isInstanceOf[Constant]) => (phi, pos)
        case (bin @ BinOp(_, arg1, op, arg2), pos)
          if (op == Plus || (op == Minus && !arg1.isInstanceOf[Constant])) // i = 1 - i is not an induction variable, its values loop.
            && ((arg1.isInstanceOf[Constant] && arg2.isInstanceOf[Register])
            || (arg1.isInstanceOf[Register] && arg2.isInstanceOf[Constant])) => (bin, pos)
      }

    // Split the instructions by phi vs binOp
    val phis = interestingInstructions.collect { case (phi: Phi, pos) => phi.dst -> (phi, pos) }.toMap
    val binOps = interestingInstructions.collect { case (binOp: BinOp, pos) => binOp.dst -> (binOp, pos) }.toMap

    // Get only phis with corresponding assignments
    val inductionPhis = phis.collect {
      case (dst, (phi, pos)) =>
        val binOpRegister = phi.cases.head.value match
          case register: Register => register
          case _ => phi.cases(1).value.asInstanceOf[Register]
        if !binOps.contains(binOpRegister) then
          None
        else
          val (binOp, binOpPos) = binOps(binOpRegister)
          if binOp.arg1 == dst || binOp.arg2 == dst then
            Some(dst -> (pos, binOpPos))
          else
            None
    }.collect {
      case Some(p) => p
    }.toMap

    inductionPhis
  }
}
