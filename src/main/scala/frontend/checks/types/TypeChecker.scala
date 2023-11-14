package frontend.checks.types

import frontend.checks.symbols.LatteType
import frontend.checks.symbols.LatteType.*
import grammar.{LatteBaseVisitor, LatteParser}

/**
 * Checks if a parse tree is type-sound and returns its type.
 */
object TypeChecker extends LatteBaseVisitor[LatteType] {

}
