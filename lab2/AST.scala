package cdim.ast

import scala.collection.mutable.ListBuffer
import cdim.token._

sealed trait Expr {
	def pretty: String
	val line: Int
	var stype: Stype
}

final case class BinaryExpr(expr1: Expr, operator: Op, expr2: Expr, var stype: Stype, val line: Int) extends Expr {
	def pretty: String = {
		"(" + expr1.pretty + " " + operator + " " + expr2.pretty + ")"
	}
}

final case class UnaryExpr(operator: Op, expr: Expr, var stype: Stype, val line: Int) extends Expr {
	def pretty: String = {
		operator.toString + " " + expr.pretty
	}
}

final case class Variable(name: String, var stype: Stype, val line: Int) extends Expr {
	def pretty: String = {
		name
	}
}

final case class ArrayElement(name: String, expr_list: ListBuffer[Expr], var stype: Stype, val line: Int) extends Expr {
	def pretty: String = {
		expr_list.foldLeft(name)((x,xs) => x + "[" + xs.pretty + "]")
	}
}

final case class Num(value: Int, var stype: Stype, val line: Int) extends Expr {
	def pretty: String = {
		value.toString
	} 
}

final case class BoolV(value: Int, var stype: Stype, val line: Int) extends Expr {
	def pretty: String = {
		if (value == 0) "false" else "true"
	} 
}

sealed trait Stmt {
	def pretty(margin: String): String
}

final case class BlockStmt(decl_list: ListBuffer[Decl], stmt: Stmt) extends Stmt {
	def pretty(margin: String): String = {
		stmt.pretty(margin + "\t")
	}
}

final case class SeqStmt(val stmt_list: ListBuffer[Stmt]) extends Stmt {
	def pretty(margin: String): String = {
		stmt_list.foldLeft("")((x, xs) => x + xs.pretty(margin))
	}
}

final case class AssignStmt(expr1: Expr, expr2: Expr) extends Stmt {
	def pretty(margin: String): String = {
		margin + expr1.pretty + " = " + expr2.pretty + "\n"
	}
}

final case class IfStmt(expr: Expr, thenStmt: Stmt, elseStmt: Stmt) extends Stmt {
	def pretty(margin: String): String = {
		val main = margin + "if " + expr.pretty + "\n" + thenStmt.pretty(margin + "\t")
		if (elseStmt == EmptyStmt) main else main + margin + "else \n" + elseStmt.pretty(margin + "\t")
	}
}

final case class WhileStmt(expr: Expr, stmt: Stmt) extends Stmt {
	def pretty(margin: String): String = {
		margin + "while " + expr.pretty + "\n" + stmt.pretty(margin + "\t")
	}
}

final case class PrintStmt(expr: Expr) extends Stmt {
	def pretty(margin: String): String = {
		margin + "print " + expr.pretty + "\n"
	}
}

final case object EmptyStmt extends Stmt {
	def pretty(margin: String): String = ""
}

sealed trait Decl {
	val name: String
	val stype: Stype
	val line: Int
}

final case class Declr(name: String, stype: Stype, line: Int) extends Decl

sealed trait Prog {
	val decl_list: ListBuffer[Decl]
	val stmt_list: ListBuffer[Stmt]
}

final case class Program(val decl_list: ListBuffer[Decl], val stmt_list: ListBuffer[Stmt]) extends Prog {
	override def toString: String = {
		stmt_list.foldLeft("")((xs,x) => xs + x.pretty(""))
	}
}

sealed trait Stype

final case object Svoid extends Stype {
	override def toString: String = "void"
}

final case object Sinteger extends Stype {
	override def toString: String = "int"
}

final case object Sbool extends Stype {
	override def toString: String = "bool"
}

final case class Sarray(length: Int, stype: Stype) extends Stype {
	override def toString: String = "array"
}
