package frontend.checks.types

import frontend.checks.symbols.ClassHierarchyCollector.HierarchyTable
import backend.generation.NamingConvention.structType

sealed trait CompilerType {
	def toLLVM: String
}

object CompilerType {
	case object CTAnyPointer extends CompilerType { override val toLLVM: String = "i8*" }
	case object CTFunctionPointer extends CompilerType { override val toLLVM: String = "void (...)*" }
	class CTFunction(val args: Seq[CompilerType], val result: CompilerType) extends CompilerType {
		override def toLLVM: String = args.map(_.toLLVM).mkString(s"${result.toLLVM}(", ",", ")") ++ "*"
	}
	case class CTPointerTo(underlying: CompilerType) extends CompilerType { override val toLLVM: String = s"${underlying.toLLVM}*"}
}

sealed trait LatteType extends CompilerType {
	/**
	 * Checks if this type is a subtype of the other type in the context given by the inheritance table.
	 */
	def isSubtypeOf(other: LatteType)(using inheritanceTable: HierarchyTable): Boolean = this == other

	/**
	 * Checks if the type is valid in the context given by known class names.
	 */
	def isValid(using classNames: Set[String]): Boolean = true

	/**
	 * Returns the type name as expected in the generated LLVM code.
	 * For classes and functions it returns their type with a pointer, because these full types are almost always referenced with a pointer.
	 */
	override def toLLVM: String = toLLVMNoPointer

	/**
	 * Same as [[toLLVM]], but does not append the pointer asterisk for classes and functions.
	 */
	def toLLVMNoPointer: String
}

object LatteType {
	sealed trait TNonFun extends LatteType

	case object TInt extends TNonFun { override val toString: String = "int"; override val toLLVMNoPointer: String = "i64" }
	case object TStr extends TNonFun {
		override val toString: String = "string"
		override val toLLVMNoPointer: String = "i8*"
	}
	case object TBool extends TNonFun { override val toString: String = "bool"; override val toLLVMNoPointer: String = "i1" }
	case object TVoid extends TNonFun { override val toString: String = "void"; override val toLLVMNoPointer: String = "void" }
	case class TClass(name: String) extends TNonFun {
		override val toString: String = s"$name"
		override def isSubtypeOf(other: LatteType)(using inheritanceTable: HierarchyTable): Boolean = other match {
			case TClass(otherClassName) => if name == otherClassName then true else inheritanceTable(name).parent match {
				case None => false
				case Some(parentClass) => TClass(parentClass).isSubtypeOf(other)
			}
			case _ => false
		}
		override def isValid(using classNames: Set[String]): Boolean = classNames.contains(name)
		override val toLLVMNoPointer: String = structType(name)
		override val toLLVM: String = s"$toLLVMNoPointer*"
	}

	case class TArray(underlying: TNonFun) extends LatteType {
		override val toString: String = s"$underlying[]"
		override def isValid(using classNames: Set[String]): Boolean = underlying.isValid
		override val toLLVMNoPointer: String = if underlying == TVoid then "i8*" else s"${underlying.toLLVM}*"
	}

	case class TFunction(override val args: Seq[LatteType], override val result: LatteType) extends CompilerType.CTFunction(args, result) with LatteType {
		override val toString: String = s"${args.mkString("(", ", ", ")")} -> $result"
		override def isSubtypeOf(other: LatteType)(using hierarchyTable: HierarchyTable): Boolean = other match {
			case TFunction(otherArgs, otherResult) =>
				args.size == otherArgs.size && args.zip(otherArgs).forall { (arg, otherArg) => otherArg.isSubtypeOf(arg) } && result.isSubtypeOf(otherResult)
			case _ => false
		}
		override def isValid(using classNames: Set[String]): Boolean = args.forall(_.isValid) && result.isValid
		override val toLLVMNoPointer: String = args.map(_.toLLVM).mkString(s"${result.toLLVM}(", ",", ")")
		override val toLLVM: String = s"$toLLVMNoPointer*"
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
	private case class MightReturn(latteType: LatteType) extends CanReturn
	case class MustReturn(latteType: LatteType) extends CanReturn with BreaksFlow
}
