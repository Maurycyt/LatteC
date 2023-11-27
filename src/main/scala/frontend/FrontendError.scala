package frontend

trait FrontendError extends Error {
	def position: Position
	def message: String
	def frontendErrorToString: String = s"At $position  :  $message"
}
