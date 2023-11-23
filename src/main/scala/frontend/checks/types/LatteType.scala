package frontend.checks.types

trait LatteType

object LatteType {
	trait TNonFunction extends LatteType // { def size: Int }

	case object TInt extends TNonFunction
	case object TStr extends TNonFunction
	case object TBool extends TNonFunction
	case object TVoid extends TNonFunction

	case class TClass(name: String) extends TNonFunction

	case class TFunction(args: Seq[TNonFunction], result: TNonFunction) extends LatteType
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

	case class MightReturn(latteType: LatteType) extends LatteStmtType {
		override def combineInSeries(other: LatteStmtType): LatteStmtType = ???
		override def combineInParallel(other: LatteStmtType): LatteStmtType = ???
	}

	case class MustReturn(latteType: LatteType) extends LatteStmtType {
		override def combineInSeries(other: LatteStmtType): LatteStmtType = ???
		override def combineInParallel(other: LatteStmtType): LatteStmtType = ???
	}

	case class Mismatch(first: LatteType, second: LatteType)
}
