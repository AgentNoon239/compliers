package cdim.ast

import scala.collection.mutable.ListBuffer
import cdim.token._

sealed trait Expr {
	def pretty: String
}

case class BinaryExpr(expr1: Expr, operator: Op, expr2: Expr) extends Expr {
	def pretty: String = {
		"(" + expr1.pretty + " " + operator + " " + expr2.pretty + ")"
	}
}
case class UnaryExpr(operator: Op, expr: Expr) extends Expr {
	def pretty: String = {
		operator.toString + " " + expr.pretty
	}
}
case class Variable(name: String) extends Expr {
	def pretty: String = {
		name
	}
}
case class Num(value: Int) extends Expr {
	def pretty: String = {
		value.toString
	} 
}

sealed trait Stmt {
	def pretty(margin: String): String
}

case class SeqStmt(list: ListBuffer[Stmt]) extends Stmt {
	def pretty(margin: String): String = {
		list.foldLeft("")((x, xs) => x + xs.pretty(margin))
	}
}

case class AssignStmt(name: String, expr: Expr) extends Stmt {
	def pretty(margin: String): String = {
		margin + name + " = " + expr.pretty + "\n"
	}
}

case class IfStmt(expr: Expr, thenStmt: Stmt, elseStmt: Stmt) extends Stmt {
	def pretty(margin: String): String = {
		val main = margin + "if " + expr.pretty + "\n" + thenStmt.pretty(margin + "   ")
		if (elseStmt == EmptyStmt) main else main + margin + "else \n" + elseStmt.pretty(margin + "   ")
	}
}

case class WhileStmt(expr: Expr, stmt: Stmt) extends Stmt {
	def pretty(margin: String): String = {
		margin + "while " + expr.pretty + "\n" + stmt.pretty(margin + "   ")
	}
}

case class PrintStmt(expr: Expr) extends Stmt {
	def pretty(margin: String): String = {
		margin + "print " + expr.pretty + "\n"
	}
}

case object EmptyStmt extends Stmt {
	def pretty(margin: String): String = ""
}

sealed trait Prog

case class Program(list: ListBuffer[Stmt]) extends Prog {
	override def toString: String = {
		list.foldLeft("")((x,xs) => x + xs.pretty(""))
	}
}