package backend.generation

import frontend.checks.symbols.ClassTable
import frontend.checks.types.CompilerType.CTAnyPointer
import frontend.checks.types.LatteType.{TBool, TInt, TStr, TVoid}

import java.io.FileWriter

object PreambleGenerator {
	def generatePreamble(using fw: FileWriter, classTable: ClassTable): Unit = {
		fw write
			s"""declare ${TVoid.toLLVM} @printInt(${TInt.toLLVM})
				 |declare ${TInt.toLLVM} @readInt()
				 |declare ${TVoid.toLLVM} @error()
				 |declare ${TVoid.toLLVM} @printString(${TStr.toLLVM})
				 |declare ${TStr.toLLVM} @readString()
				 |
				 |declare ${TStr.toLLVM} @concatenateStrings(${TStr.toLLVM}, ${TStr.toLLVM})
				 |declare ${TBool.toLLVM} @compareStrings(${TStr.toLLVM}, ${TStr.toLLVM})
				 |
				 |declare ${CTAnyPointer.toLLVM} @calloc(${TInt.toLLVM}, ${TInt.toLLVM})
				 |declare ${TVoid.toLLVM} @free(${CTAnyPointer.toLLVM})
				 |
				 |declare ${TVoid.toLLVM} @registerString(${CTAnyPointer.toLLVM})
				 |declare ${TVoid.toLLVM} @registerArray(${CTAnyPointer.toLLVM}, ${TBool.toLLVM})
				 |declare ${TVoid.toLLVM} @registerObject(${CTAnyPointer.toLLVM}, ${TInt.toLLVM})
				 |declare ${TVoid.toLLVM} @registerObjectPointerMemberOffset(${TInt.toLLVM}, ${TInt.toLLVM})
				 |
				 |declare ${TVoid.toLLVM} @increaseRefCount(${CTAnyPointer.toLLVM})
				 |declare ${TVoid.toLLVM} @decreaseRefCount(${CTAnyPointer.toLLVM})
				 |declare ${TVoid.toLLVM} @clearUnboundPointers()
				 |
				 |define i32 @main() {
				 |""".stripMargin
		classTable.keys.foreach { className =>
			fw write s"\tcall void ${NamingConvention.offsetCollector(className)}()\n"
		}
		if classTable.nonEmpty then fw write "\n"
		fw write
			s"""\t%result64 = call ${TInt.toLLVM} ${NamingConvention.function("main")}()
			   |\t%result32 = trunc ${TInt.toLLVM} %result64 to i32
			   |\tret i32 %result32
			   |}
			   |""".stripMargin
	}
}
