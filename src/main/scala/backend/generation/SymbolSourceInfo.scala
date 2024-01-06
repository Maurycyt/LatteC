package backend.generation

import frontend.checks.symbols.SymbolInterface
import frontend.checks.types.{CompilerType, LatteType}
import frontend.Position

/**
 * Contains all the necessary information about a symbol in the context of static checks.
 * @param symbolName          The name of the symbol.
 * @param hostClass           The possible class in which this symbol is defined. Especially helpful when handling methods.
 * @param source              Where to take the value of the symbol from.
 */
class SymbolSourceInfo(val symbolName: String, val hostClass: Option[String], var source: Source) extends SymbolInterface {
	override def declarationPosition: Position = Position(0, 0) // Whatever, we won't be using it. I know, this is *slightly* spaghetti.
	override def symbolType: CompilerType = source.valueType

	def this(info: SymbolInterface) = this(info.symbolName, None, Undefined(info.symbolType))
}
