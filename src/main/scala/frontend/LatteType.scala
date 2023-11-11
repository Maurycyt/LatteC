package frontend

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
