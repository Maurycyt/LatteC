package frontend.checks.types

import frontend.{FrontendError, Position}

case class UnknownTypeError(position: Position, givenType: LatteType) extends FrontendError {
	val message: String = s"Unknown type: $givenType."
}
