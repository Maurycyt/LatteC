package backend.generation

import frontend.checks.symbols.{ClassTable, MemberTable, SymbolInfo}
import NamingConvention.*
import frontend.checks.types.LatteType
import frontend.checks.types.LatteType.*

import java.io.FileWriter

object ClassRepresentationBuilder {
	def buildClasses(using classTable: ClassTable, fw: FileWriter): Unit = {
		classTable.keys.toList match {
			case Nil => ()
			case h :: t =>
				buildClassRepresentation(h)
				t.foreach { className =>
					fw write "\n\n"
					buildClassRepresentation(className)
				}
		}
	}

	private def buildClassRepresentation(className: String)(using classTable: ClassTable, fw: FileWriter): Unit = {
		given String = className
		given MemberTable = classTable(className).memberTable
		buildStructType
		fw write "\n\n"
		buildVTableDefinition
		fw write "\n\n"
		buildConstructor
	}

	private def buildStructType(using className: String, memberTable: MemberTable, fw: FileWriter): Unit = {
		fw write memberTable.fields.map(_.symbolInfo)
			.map { fieldSymbol => s"\t${fieldSymbol.symbolType.toLLVM}" }
			.prepended(s"\ti8*") // The vTable comes first.
			.mkString(s"${structType(className)} = type {\n", ",\n", "\n}")
	}

	private def buildVTableDefinition(using className: String, memberTable: MemberTable, fw: FileWriter): Unit = {
		val methodTable: Seq[SymbolInfo] = memberTable.methods.map(_.symbolInfo)
		val numFunctions = memberTable.numFunctions

		fw write methodTable
			.map { methodSymbol =>
				s"\tvoid()* bitcast (${methodSymbol.symbolType.toLLVM} ${method(className, methodSymbol.symbolName)} to void()*)"
			}
			.mkString(s"${vTable(className)} = global [$numFunctions x void()*] [\n", ",\n", "\n]")
	}

	private def buildConstructor(using className: String, memberTable: MemberTable, fw: FileWriter): Unit = {
		// Signature
		fw write s"define void ${constructor(className)}(${structType(className)}* %this) nounwind {\n"

		// Initialise the vtable.
		fw write s"\t%vTable.ptr.ptr = getelementptr ${structType(className)}, ${structType(className)}* %this, i32 0, i32 0\n"
		fw write s"\t%vTable.ptr = bitcast [${memberTable.numFunctions} x void()*]* ${vTable(className)} to i8*\n"
		fw write s"\tstore i8* %vTable.ptr, i8** %vTable.ptr.ptr\n"

		// Initialise the other fields.
		memberTable.fields.filterNot(_.symbolInfo.symbolType == TVoid)
			.foreach { fieldMember =>
				// The first line obtains the pointer to the field.
				fw write s"\t%member.${fieldMember.offset}.ptr = getelementptr ${structType(className)}, ${structType(className)}* %this, i32 0, i32 ${fieldMember.offset}\n"

				// The next lines optionally allocate and construct an object.
				fieldMember.symbolInfo.symbolType match {
					case TClass(fieldClassName) =>
						fw write s"\t%member.${fieldMember.offset}.object = alloca ${structType(fieldClassName)}\n"
						fw write s"\tcall void ${constructor(fieldClassName)}(${structType(fieldClassName)}* %member.${fieldMember.offset}.object)\n"
					case _ => ()
				}

				// The last line stores the appropriate value under the pointer to the field.
				fieldMember.symbolInfo.symbolType match {
					case t @ TInt  => fw write s"\tstore ${t.toLLVM} 0, ${t.toLLVM}* %member.${fieldMember.offset}.ptr\n"
					case t @ TBool => fw write s"\tstore ${t.toLLVM} 0, ${t.toLLVM}* %member.${fieldMember.offset}.ptr\n"
					case t => fw write s"\tstore ${t.toLLVM} %member.${fieldMember.offset}.object, ${t.toLLVM}* %member.${fieldMember.offset}.ptr\n"
				}
			}

		// Exit.
		fw write "\tret void\n}"
	}
}
