package cdim.error

case class ParserException(message: String) extends Exception(message)
case object NoSuchElementException extends Exception