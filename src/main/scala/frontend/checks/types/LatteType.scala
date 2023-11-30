package frontend.checks.types

import frontend.checks.symbols.ClassTable

trait LatteType {
	def isSubtypeOf(other: LatteType)(implicit classTable: ClassTable): Boolean = this == other
}

object LatteType {
	sealed trait TBasic extends LatteType // { def size: Int }

	case object TInt extends TBasic { override val toString: String = "int" }
	case object TStr extends TBasic { override val toString: String = "str" }
	case object TBool extends TBasic { override val toString: String = "bool" }
	case object TVoid extends TBasic { override val toString: String = "void" }
	case class TClass(name: String) extends TBasic {
		override val toString: String = s"$name"
		override def isSubtypeOf(other: LatteType)(using classTable: ClassTable): Boolean = other match {
			case TClass(otherClass) => if name == otherClass then true else classTable(name)._2 match {
				case None => false
				case Some(parentClass) => TClass(parentClass).isSubtypeOf(other)
			}
			case _ => false
		}
	}

	case class TArray(underlying: TBasic) extends LatteType { override val toString: String = s"$underlying[]" }

	case class TFunction(args: Seq[LatteType], result: LatteType) extends LatteType {
		override val toString: String = s"${args.mkString("(", ", ", ")")} -> $result"
		override def isSubtypeOf(other: LatteType)(implicit classTable: ClassTable): Boolean = other match {
			case TFunction(otherArgs, otherResult) =>
				args.size == otherArgs.size && args.zip(otherArgs).forall { (arg, otherArg) => otherArg.isSubtypeOf(arg) } && result.isSubtypeOf(otherResult)
			case _ => false
		}
	}
}

sealed trait LatteStmtType {
	def loseConfidence: LatteStmtType = this
}

object LatteStmtType {
	sealed trait DoesNotReturn extends LatteStmtType {
		override def loseConfidence: LatteStmtType = Ignored
	}
	sealed trait CanReturn extends LatteStmtType {
		def latteType: LatteType
		override def loseConfidence: LatteStmtType = MightReturn(latteType)
	}
	sealed trait BreaksFlow extends LatteStmtType

	case object Ignored extends DoesNotReturn
	case object Loops extends DoesNotReturn with BreaksFlow
	case class MightReturn(latteType: LatteType) extends CanReturn
	case class MustReturn(latteType: LatteType) extends CanReturn with BreaksFlow
}
