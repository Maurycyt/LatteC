package backend.optimisation

import backend.representation.Function

object Optimiser {
  def optimiseFunction(function: Function, inlinableFunctions: Map[String, Function]): Unit = {
    var stop = false
    given Function = function
    given Map[String, Function] = inlinableFunctions
    while !stop do {
      stop = !BasicOptimiser.optimiseFunction && !CommonSubexpressionOptimiser.optimiseFunction && !FunctionInliner.inlineFunction
    }
  }
}
