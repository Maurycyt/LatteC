package backend.generation

import frontend.checks.symbols.{ClassTable, MemberInfo, MemberTable, SymbolInfo}
import NamingConvention.*
import frontend.checks.types.LatteType
import frontend.checks.types.LatteType.*
import backend.representation.{CallVoid, Constant, Function, GetElementPtr, Label, Literal, PtrToInt, Register, ReturnVoid}
import frontend.checks.types.CompilerType.{CTAnyPointer, CTFunction, CTPointerTo}

import java.io.FileWriter
import scala.collection.mutable

object ClassRepresentationBuilder {
	def buildClasses(using classTable: ClassTable, fw: FileWriter): Seq[Function] = {
		classTable.keys.toList match {
			case Nil => Seq.empty
			case h :: t =>
				val firstFunctionPair = buildClassRepresentation(h)
				t.foldLeft(Seq(firstFunctionPair._2, firstFunctionPair._1)) { (readyConstructors, className) =>
					fw write "\n\n"
					val nextFunctionPair = buildClassRepresentation(className)
					nextFunctionPair._2 +: nextFunctionPair._1 +: readyConstructors
				}.reverse
		}
	}

	private def buildClassRepresentation(className: String)(using classTable: ClassTable, fw: FileWriter): (Function, Function) = {
		given String = className
		given Long = classTable(className).classID
		given MemberTable = classTable(className).memberTable
		buildStructType
		fw write "\n\n"
		buildVTableDefinition
		(buildConstructor, buildOffsetCollector)
	}

	private def buildStructType(using className: String, memberTable: MemberTable, fw: FileWriter): Unit = {
		fw write memberTable.fields.map(_.symbolInfo)
			.collect { case fieldSymbol if fieldSymbol.symbolType != TVoid => s"\t${fieldSymbol.symbolType.toLLVM}" }
			.prepended(s"\ti8*") // The vTable comes first.
			.mkString(s"${structType(className)} = type {\n", ",\n", "\n}\n")
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

	private def buildConstructor(using className: String, memberTable: MemberTable): Function = {
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

	private def buildOffsetCollector(using className: String, classID: Long, memberTable: MemberTable): Function = {
		// Create the function object.
		val function = Function(
			offsetCollector(className),
			offsetCollector(className),
			TVoid,
			Seq(),
			mutable.HashMap.empty,
			None,
			NameGenerator()
		)
		val block = function.addBlock(Some("entry"))

		// Add offsets for all pointer members.
		memberTable.fields.foreach { info =>
			if info.symbolType.isInstanceOf[TStr.type | TArray | TClass] then
				val memberRelativePointer = Register(CTPointerTo(info.symbolType), function.nameGenerator.nextRegister)
				block += GetElementPtr(memberRelativePointer, Constant(TClass(className), 0), Constant(TInt, 0), Constant(TInt, info.offset))
				val memberOffset = Register(TInt, function.nameGenerator.nextRegister)
				block += PtrToInt(memberOffset, memberRelativePointer)
				block += CallVoid(Label(CTFunction(Seq(TInt, TInt), TVoid), "@registerObjectPointerMemberOffset"), Constant(TInt, classID), memberOffset)
		}

		// Exit.
		block += ReturnVoid
		function
	}
}
