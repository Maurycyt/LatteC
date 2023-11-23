package frontend.checks.types

trait LatteType

object LatteType {
	trait TBasic extends LatteType // { def size: Int }

	case object TInt extends TBasic { override val toString: String = "int" }
	case object TStr extends TBasic { override val toString: String = "str" }
	case object TBool extends TBasic { override val toString: String = "bool" }
	case object TVoid extends TBasic { override val toString: String = "void" }
	case class TClass(name: String) extends TBasic { override val toString: String = s"$name" }

	case class TArray(underlying: TBasic) extends LatteType { override val toString: String = s"$underlying[]" }
	case class TFunction(args: Seq[LatteType], result: LatteType) extends LatteType { override val toString: String = s"${args.mkString("(", ", ", ")")} -> $result" }
}

trait LatteStmtType {
	// For use in block statements
	def combineInSeries(other: LatteStmtType): LatteStmtType
	// For use in branching statements
	def combineInParallel(other: LatteStmtType): LatteStmtType
}

object LatteStmtType {
	case object Ignored extends LatteStmtType {
		override def combineInParallel(alternative: LatteStmtType): LatteStmtType = alternative match {
			case MustReturn(t) => MightReturn(t)
			case other         => other
		}
		override def combineInSeries(other: LatteStmtType): LatteStmtType = other
	}

	trait CanReturn extends LatteStmtType {
		def latteType: LatteType
	}

	case class MightReturn(latteType: LatteType) extends CanReturn {
		override def combineInSeries(other: LatteStmtType): LatteStmtType = ???
		override def combineInParallel(other: LatteStmtType): LatteStmtType = ???
	}

	case class MustReturn(latteType: LatteType) extends CanReturn {
		override def combineInSeries(other: LatteStmtType): LatteStmtType = ???
		override def combineInParallel(other: LatteStmtType): LatteStmtType = ???
	}

	case class Mismatch(first: LatteType, second: LatteType)
}
