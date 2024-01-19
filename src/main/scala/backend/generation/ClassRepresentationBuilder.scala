package backend.generation

import frontend.checks.symbols.{ClassTable, MemberInfo, MemberTable, SymbolInfo}
import NamingConvention.*
import frontend.checks.types.LatteType
import frontend.checks.types.LatteType.*
import backend.representation.{Function, Literal, Register, ReturnVoid}

import java.io.FileWriter
import scala.collection.mutable

object ClassRepresentationBuilder {
	def buildClasses(using classTable: ClassTable, fw: FileWriter): Set[Function] = {
		classTable.keys.toList match {
			case Nil => Set.empty
			case h :: t =>
				val firstConstructor = buildClassRepresentation(h)
				t.foldLeft(Set(firstConstructor)) { (readyConstructors, className) =>
					fw write "\n\n"
					readyConstructors + buildClassRepresentation(className)
				}
		}
	}

	private def buildClassRepresentation(className: String)(using classTable: ClassTable, fw: FileWriter): Function = {
		given String = className
		given MemberTable = classTable(className).memberTable
		buildStructType
		fw write "\n\n"
		buildVTableDefinition
		buildConstructor
	}

	private def buildStructType(using className: String, memberTable: MemberTable, fw: FileWriter): Unit = {
		fw write memberTable.fields.map(_.symbolInfo)
			.collect { case fieldSymbol if fieldSymbol.symbolType != TVoid => s"\t${fieldSymbol.symbolType.toLLVM}" }
			.prepended(s"\ti8*") // The vTable comes first.
			.mkString(s"${structType(className)} = type {\n", ",\n", "\n}")
	}

	private def buildVTableDefinition(using className: String, memberTable: MemberTable, fw: FileWriter): Unit = {
		val methodTable: Seq[MemberInfo] = memberTable.methods
		val numFunctions = memberTable.numFunctions

		fw write methodTable
			.map { methodSymbol =>
				val methodTypeWithoutSelf = methodSymbol.symbolType.asInstanceOf[TFunction]
				val methodTypeWithSelf = methodTypeWithoutSelf.copy(args = methodTypeWithoutSelf.args.prepended(TClass(methodSymbol.hostClass)))
				s"\n\tvoid()* bitcast (${methodTypeWithSelf.toLLVM} ${method(methodSymbol.hostClass, methodSymbol.symbolName)} to void()*)"
			}
			.mkString(s"${vTable(className)} = global [$numFunctions x void()*] [", ",", "\n]\n")
	}

	private def buildConstructor(using className: String, memberTable: MemberTable, fw: FileWriter): Function = {
		// Create the function object.
		val thisRegister = Register(TClass(className), "%this")
		val function = Function(
			constructor(className),
			constructor(className),
			TVoid,
			Seq("%this"),
			mutable.HashMap("%this" -> SymbolSourceInfo("%this", None, thisRegister)),
			None,
			NameGenerator()
		)
		val block = function.addBlock(Some("entry"))

		// Initialise the vtable.
		val tClass = TClass(className)
		// We use literal instructions here due to the unconventional types.
		block += Literal(s"%vTable.ptr.ptr = getelementptr ${tClass.toLLVMNoPointer}, ${tClass.toLLVM} %this, i32 0, i32 0")
		block += Literal(s"%vTable.ptr = bitcast [${memberTable.numFunctions} x void()*]* ${vTable(className)} to i8*")
		block += Literal(s"store i8* %vTable.ptr, i8** %vTable.ptr.ptr")

		// Exit.
		block += ReturnVoid
		function
	}
}
