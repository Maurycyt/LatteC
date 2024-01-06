package backend.generation

import frontend.checks.types.LatteType.TStr

import java.io.FileWriter

object PreambleGenerator {
	def generatePreamble(using fw: FileWriter): Unit = {
		fw write
			s"""declare void @printInt(i32)
				 |declare i32 @readInt()
				 |declare void @error()
				 |declare void @printString(${TStr.toLLVM})
				 |declare ${TStr.toLLVM} @readString()
				 |
				 |declare ${TStr.toLLVM} @concatenateStrings(${TStr.toLLVM}, ${TStr.toLLVM})
				 |
				 |declare i8* @malloc(i64)
				 |declare void @free(i8*)
				 |""".stripMargin
	}
}
