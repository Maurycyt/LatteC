package backend.generation

import frontend.checks.symbols.{ClassTable, MemberTable, SymbolInfo}
import NamingConvention.*
import frontend.checks.types.LatteType
import frontend.checks.types.LatteType.*

object ClassRepresentationBuilder {
	def buildClasses(using classTable: ClassTable, sb: StringBuilder): Unit = {
		classTable.keys.toList match {
			case Nil => ()
			case h :: t =>
				buildClassRepresentation(h)
				t.foreach { className =>
					sb ++= "\n\n"
					buildClassRepresentation(className)
				}
		}
	}

	private def buildClassRepresentation(className: String)(using classTable: ClassTable, sb: StringBuilder): Unit = {
		given String = className
		given MemberTable = classTable(className).memberTable
		buildStructType
		sb ++= "\n\n"
		buildVTableDefinition
		sb ++= "\n\n"
		buildConstructor
	}

	private def buildStructType(using className: String, memberTable: MemberTable, sb: StringBuilder): Unit = {
		sb ++= memberTable.fields.map(_.symbolInfo)
			.map { fieldSymbol => s"\t${fieldSymbol.symbolType.toLLVM}" }
			.prepended(s"\t${vTableType(className)}*") // The vTable comes first.
			.mkString(s"${structType(className)} = type {\n", ",\n", "\n}")
	}

	private def buildVTableDefinition(using className: String, memberTable: MemberTable, sb: StringBuilder): Unit = {
		val methodTable: Seq[SymbolInfo] = memberTable.methods.map(_.symbolInfo)
		val vTableTypeInLLVM: String = methodTable.map(_.symbolType.toLLVM).mkString(s"${vTableType(className)} = type {\n\t", ",\n\t", "\n}")
		val vTableDefinitionInLLVM: String =
			methodTable
				.map { methodSymbol => s"\t${methodSymbol.symbolType.toLLVM} ${method(methodSymbol)}"}
				.mkString(s"${vTableData(className)} = global ${vTableType(className)} {\n", ",\n", "\n}")

		sb ++= vTableTypeInLLVM
		sb ++= "\n\n"
		sb ++= vTableDefinitionInLLVM
	}

	private def buildConstructor(using className: String, memberTable: MemberTable, sb: StringBuilder): Unit = {
		// Signature
		sb ++= s"define void ${constructor(className)}(${structType(className)}* %this) nounwind {\n"

		// Initialise the vtable.
		sb ++= s"\t%vTable.ptr = getelementptr ${structType(className)}, ${structType(className)}* %this, i32 0, i32 0\n"
		sb ++= s"\tstore ${vTableType(className)}* ${vTableData(className)}, ${vTableType(className)}** %vTable.ptr\n"

		// Initialise the other fields.
		memberTable.fields.filterNot(_.symbolInfo.symbolType == TVoid)
			.foreach { fieldMember =>
				// The first line obtains the pointer to the field.
				sb ++= s"\t%member.${fieldMember.index}.ptr = getelementptr ${structType(className)}, ${structType(className)}* %this, i32 0, i32 ${fieldMember.index}\n"

				// The next lines optionally allocate and construct an object.
				fieldMember.symbolInfo.symbolType match {
					case TClass(fieldClassName) =>
						sb ++= s"\t%member.${fieldMember.index}.object = alloca ${structType(fieldClassName)}\n"
						sb ++= s"\tcall void ${constructor(fieldClassName)}(${structType(fieldClassName)}* %member.${fieldMember.index}.object)\n"
					case _ => ()
				}

				// The last line stores the appropriate value under the pointer to the field.
				fieldMember.symbolInfo.symbolType match {
					case t @ TInt  => sb ++= s"\t store ${t.toLLVM} 0, ${t.toLLVM}* %member.${fieldMember.index}.ptr\n"
					case t @ TBool => sb ++= s"\t store ${t.toLLVM} 0, ${t.toLLVM}* %member.${fieldMember.index}.ptr\n"
					case t => sb ++= s"\tstore ${t.toLLVM} %member.${fieldMember.index}.object, ${t.toLLVM}* %member.${fieldMember.index}.ptr\n"
				}
			}

		// Exit.
		sb ++= "\tret void\n}"
	}
}
