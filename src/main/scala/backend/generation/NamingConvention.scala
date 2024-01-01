package backend.generation

//sealed trait ExternFunction
//case object

object NamingConvention {
	val self: String = "%self"
	def function(functionName: String): String = s"@function.$functionName"
	def structType(className: String): String = s"%struct.$className"
	def method(className: String, methodName: String): String = s"@method.$className.$methodName"
	def vTable(className: String): String = s"@vTable.$className"
	def constructor(className: String): String = s"@constructor.$className"

	// The number of dots corresponds with the number of shadows of the given name.
	// An additional "copy" parameter specifies which
//	def variable(name: String, copy: Int = 0)(using symbolStack: SymbolStack): String = "%var" + "." * symbolStack.count(name) + s"$name.$copy"
}
