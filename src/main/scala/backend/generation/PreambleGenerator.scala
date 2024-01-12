package backend.generation

import frontend.checks.types.CompilerType.AnyPointer
import frontend.checks.types.LatteType.{TBool, TInt, TStr, TVoid}

import java.io.FileWriter

object PreambleGenerator {
	def generatePreamble(using fw: FileWriter): Unit = {
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
				 |declare ${AnyPointer.toLLVM} @malloc(${TInt.toLLVM})
				 |declare ${TVoid.toLLVM} @free(${AnyPointer.toLLVM})
				 |
				 |define i32 @main() {
				 |\t%result64 = call ${TInt.toLLVM} ${NamingConvention.function("main")}()
				 |\t%result32 = trunc ${TInt.toLLVM} %result64 to i32
				 |\tret i32 %result32
				 |}
				 |""".stripMargin
	}
}
