package frontend.checks

import frontend.Position

trait FrontendError extends Error {
	def position: Position
	def message: String
	def frontendErrorToString: String = s"At $position  :  $message"
}

case class RedeclarationError(position: Position, name: String) extends FrontendError {
	override def message: String = s"Redeclaration of symbol $name."
}
