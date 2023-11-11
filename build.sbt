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
		  val depsClasspath = (Compile / dependencyClasspath).value.map(_.data).mkString(".:", ":", "")

		  val cachedFunction = FileFunction.cached(
			  streams.value.cacheDirectory / "grammar"
		  ) { (in: Set[File]) =>
			  val grammarPath = in.head.toPath
			  print("Generating grammar files... ")
			  Process(Seq("java", "-cp", depsClasspath, "org.antlr.v4.Tool", grammarPath.toString, "-no-listener", "-visitor", "-package", "grammar", "-o", s"${baseDirectory.value}/src/main/java/grammar")).!
			  println("Done.")
			  IO.write(Path("dependencies.cp").asFile, depsClasspath)

			  generatedGrammarFiles(baseDirectory.value).filter(_.toString.endsWith(".java")).toSet
		  }

		  val inputFile = file(s"src/main/resources/$languageName.g4")

		  cachedFunction(Set(inputFile)).toSeq
	  },

	  cleanFiles ++= generatedGrammarFiles(baseDirectory.value)
  )
