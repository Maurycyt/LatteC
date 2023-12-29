package backend.generation

import frontend.checks.symbols.SymbolInfo

object NamingConvention {
	def function(functionName: String): String = s"@function.$functionName"
	def structType(className: String): String = s"%struct.$className"
	def method(methodSymbol: SymbolInfo): String = s"@method.${methodSymbol.hostClass.get}.${methodSymbol.symbolName}"
	def vTableType(className: String): String = s"%vTableT.$className"
	def vTableData(className: String): String = s"@vTable.$className"
	def constructor(className: String): String = s"@constructor.$className"
}
