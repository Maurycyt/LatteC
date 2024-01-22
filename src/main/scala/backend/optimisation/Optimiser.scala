package backend.optimisation

import backend.representation.Function

object Optimiser {
  def optimiseFunction(function: Function): Unit = {
    var stop = false
    given Function = function
    while !stop do {
      stop = !BasicOptimiser.optimiseFunction && !CommonSubexpressionOptimiser.optimiseFunction
    }
  }
}
