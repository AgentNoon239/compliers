package cdim.token

import cdim.ast.Stype

sealed trait Op {
	def relop: Boolean
	def keiko: String
}

final case object BANG_EQUAL extends Op {
	override def toString: String = "!="
	def relop = true
	def keiko = "NEQ"
}
final case object EQUAL_EQUAL extends Op {
	override def toString: String = "=="
	def relop = true
	def keiko = "EQ"
}
final case object LESS extends Op {
	override def toString: String = "<"
	def relop = true
	def keiko = "LT"
}
final case object LESS_EQUAL extends Op {
	override def toString: String = "<="
	def relop = true
	def keiko = "LEQ"
}
final case object GREATER extends Op {
	override def toString: String = ">"
	def relop = true
	def keiko = "GT"
}
final case object GREATER_EQUAL extends Op {
	override def toString: String = ">="
	def relop = true
	def keiko = "GEQ"
}
final case object UMINUS extends Op {
	override def toString: String = "-"
	def relop = false
	def keiko = "UMINUS"
}
final case object AND extends Op {
	override def toString: String = "&&"
	def relop = false
	def keiko = "AND"
}
final case object OR extends Op {
	override def toString: String = "||"
	def relop = false
	def keiko = "OR"
}
final case object SUBTRACT extends Op {
	override def toString: String = "-"
	def relop = false
	def keiko = "MINUS"
}
final case object ADD extends Op {
	override def toString: String = "+"
	def relop = false
	def keiko = "PLUS"
}
final case object DIV extends Op {
	override def toString: String = "/"
	def relop = false
	def keiko = "DIV"
}
final case object TIMES extends Op {
	override def toString: String = "*"
	def relop = false
	def keiko = "TIMES"
}
final case object NOT extends Op {
	override def toString: String = "!"
	def relop = false
	def keiko = "NOT"
}

sealed trait Token

final case object LEFT_PAREN extends Token {
	override def toString: String = "("
}
final case object RIGHT_PAREN extends Token {
	override def toString: String = ")"
}
final case object LEFT_BRACE extends Token {
	override def toString: String = "{"
}
final case object RIGHT_BRACE extends Token {
	override def toString: String = "}"
}
final case object LEFT_SQ extends Token {
	override def toString: String = "["
}
final case object RIGHT_SQ extends Token {
	override def toString: String = "]"
}
final case object MINUS extends Token
final case object SEMICOLON extends Token {
	override def toString: String = ";"
}
final case object COMMA extends Token {
	override def toString: String = ","
}
final case object QUESTION extends Token
final case object COLON extends Token
final case object BANG extends Token
final case object EQUAL extends Token {
	override def toString: String = "="
}
final case class RELOP(operation: Op) extends Token
final case class BINOP(operation: Op) extends Token
final case class IDENT(name: String) extends Token
final case class NUMBER(value: Int) extends Token
final case class TYPE(stype: Stype) extends Token
final case object TRUE extends Token
final case object FALSE extends Token
final case object IF extends Token
final case object WHILE extends Token
final case object ELSE extends Token
final case object FOR extends Token
final case object PRINT extends Token
final case object EOF extends Token

