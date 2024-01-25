package backend.optimisation

import backend.representation.Function

object Optimiser {
  def optimiseFunctions(functions: Seq[Function], inlinableFunctions: Map[String, Function]): Unit = {
    if functions.isEmpty then return
    var stop = false
    given Map[String, Function] = inlinableFunctions
    while !stop do {
      var somethingChanged = false
      for (function <- functions) do {
        given Function = function
        somethingChanged |= FunctionInliner.inlineFunction
        somethingChanged |= BasicOptimiser.optimiseFunction
        somethingChanged |= CommonSubexpressionOptimiser.optimiseFunction
        somethingChanged |= StrengthReducer.optimiseFunction
      }
      if !somethingChanged then stop = true
    }
  }
}
