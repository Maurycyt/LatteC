import scala.sys.process.Process

ThisBuild / version := "1"
ThisBuild / scalaVersion := "3.3.1"

libraryDependencies += "org.antlr" % "antlr4" % "4.13.1"

val languageName = "Latte"
def generatedGrammarFiles(baseDirectoryFile: File): Seq[File] = {
	Seq(".interp", ".tokens", "BaseVisitor.java", "Lexer.java", "Lexer.interp", "Lexer.tokens", "Parser.java", "Visitor.java").map {
		suffix => baseDirectoryFile / "src" / "main" / "java" / "grammar" / (languageName + suffix)
	}
}

lazy val root = (project in file("."))
  .settings(
    name := s"${languageName}C",

	  run / fork := true,

	  // Generates the lexer and parser.
	  Compile / sourceGenerators += Def.task {
		  val extraClasspath = (Compile / dependencyClasspath).value.map(_.data).mkString(".:", ":", "")

		  val cachedFunction = FileFunction.cached(
			  streams.value.cacheDirectory / "grammar"
		  ) { (in: Set[File]) =>
			  val grammarPath = in.head.toPath
			  print("Generating grammar files... ")
			  val res = Process(Seq("java", "-cp", extraClasspath, "org.antlr.v4.Tool", grammarPath.toString, "-no-listener", "-visitor", "-package", "grammar", "-o", s"${grammarPath.getParent.getParent}/java/grammar")).!
			  println("Done.")
			  IO.write(Path("dependencies-classpath.cp").asFile, extraClasspath)

			  generatedGrammarFiles(baseDirectory.value).filter(_.toString.endsWith(".java")).toSet
		  }

		  val inputFile = file(s"src/main/resources/$languageName.g4")

		  cachedFunction(Set(inputFile)).toSeq
	  },

	  cleanFiles ++= generatedGrammarFiles(baseDirectory.value)
  )
