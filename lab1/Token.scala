package cdim.token

sealed trait Op

case object BANG_EQUAL extends Op
case object EQUAL_EQUAL extends Op
case object LESS extends Op
case object LESS_EQUAL extends Op
case object GREATER extends Op
case object GREATER_EQUAL extends Op
case object UMINUS extends Op
case object AND extends Op
case object OR extends Op
case object SUBTRACT extends Op
case object ADD extends Op
case object DIV extends Op
case object TIMES extends Op
case object NOT extends Op

sealed trait Token

case object LEFT_PAREN extends Token {
	override def toString: String = "("
}
case object RIGHT_PAREN extends Token {
	override def toString: String = ")"
}
case object LEFT_BRACE extends Token {
	override def toString: String = "{"
}
case object RIGHT_BRACE extends Token {
	override def toString: String = "}"
}
case object MINUS extends Token
case object SEMICOLON extends Token {
	override def toString: String = ";"
}
case object QUESTION_MARK extends Token {
	override def toString: String = "?"
}
case object COLON extends Token {
	override def toString: String = ":"
}
case object BANG extends Token
case object EQUAL extends Token {
	override def toString: String = "="
}
case class RELOP(operation: Op) extends Token
case class BINOP(operation: Op) extends Token
case class IDENT(name: String) extends Token
case class NUMBER(value: Int) extends Token

case object IF extends Token
case object WHILE extends Token
case object ELSE extends Token
case object FOR extends Token
case object PRINT extends Token

case object EOF extends Token
