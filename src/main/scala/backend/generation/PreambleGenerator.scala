package backend.generation

import frontend.checks.types.LatteType.TStr

import java.io.FileWriter

object PreambleGenerator {
	def generatePreamble(using fw: FileWriter): Unit = {
		fw write
			s"""declare void @printInt(i64)
				 |declare i64 @readInt()
				 |declare void @error()
				 |declare void @printString(${TStr.toLLVM})
				 |declare ${TStr.toLLVM} @readString()
				 |
				 |declare ${TStr.toLLVM} @concatenateStrings(${TStr.toLLVM}, ${TStr.toLLVM})
				 |
				 |declare i8* @malloc(i64)
				 |declare void @free(i8*)
				 |
				 |define i32 @main() {
				 |\t%result64 = call i64 ${NamingConvention.function("main")}()
				 |\t%result32 = trunc i64 %result64 to i32
				 |\tret i32 %result32
				 |}
				 |""".stripMargin
	}
}
