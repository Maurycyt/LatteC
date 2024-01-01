package backend.generation

import frontend.checks.symbols.SymbolInfo

//sealed trait ExternFunction
//case object

object NamingConvention {
	def function(functionName: String): String = s"@function.$functionName"
//	def externFunction(externFunction: ExternFunction)
	def structType(className: String): String = s"%struct.$className"
	def method(methodSymbol: SymbolInfo): String = s"@method.${methodSymbol.hostClass.get}.${methodSymbol.symbolName}"
	def vTableType(className: String): String = s"%vTableT.$className"
	def vTableData(className: String): String = s"@vTable.$className"
	def constructor(className: String): String = s"@constructor.$className"

	// The number of dots corresponds with the number of shadows of the given name.
	// An additional "copy" parameter specifies which
//	def variable(name: String, copy: Int = 0)(using symbolStack: SymbolStack): String = "%var" + "." * symbolStack.count(name) + s"$name.$copy"
}
