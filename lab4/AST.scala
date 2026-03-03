package cdim.ast

import scala.collection.mutable.ListBuffer
import cdim.token._

sealed trait Expr {
	def pretty: String
	val line: Int
	var stype: Stype
	var name: String
}

final case class BinaryExpr(expr1: Expr, operator: Op, expr2: Expr, var stype: Stype, line: Int) extends Expr {
	def pretty: String = {
		"(" + expr1.pretty + " " + operator + " " + expr2.pretty + ")"
	}
	var name = ""
}

final case class UnaryExpr(operator: Op, expr: Expr, var stype: Stype, line: Int) extends Expr {
	def pretty: String = {
		operator.toString + " " + expr.pretty
	}
	var name = ""
}

final case class Variable(var name: String, var stype: Stype, line: Int) extends Expr {
	def pretty: String = {
		name
	}
}

final case class FunCall(var name: String, arg_list: ListBuffer[Expr], var stype: Stype, line: Int) extends Expr {
	def pretty: String = arg_list.foldLeft(name + "(")((x,xs) => x + " " + xs.pretty) + ")"
}

final case class Num(value: Int, var stype: Stype, line: Int) extends Expr {
	def pretty: String = {
		value.toString
	} 
	var name = ""
}

final case class BoolV(value: Int, var stype: Stype, line: Int) extends Expr {
	def pretty: String = {
		if (value == 0) "false" else "true"
	} 
	var name = ""
}

final case class Nil(var stype: Stype, line: Int) extends Expr {
	def pretty: String = "void"
	var name = ""
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

final case class ReturnStmt(expr: Expr) extends Stmt {
	def pretty(margin: String): String = {
		margin + "return " + expr.pretty + "\n"
	}
}

sealed trait Decl {
	val name: String
	val stype: Stype
	val line: Int
}

final case class VarDecl(name: String, stype: Stype, line: Int) extends Decl {
	override def toString: String = ""
}

final case class FunDecl(name: String, param_list: ListBuffer[Paren], block: Stmt, line: Int) extends Decl {
	override def toString: String = name + ":\n" + block.pretty("")
	val stype = Sinteger
}

sealed trait Paren {
	def get_name: String
}

final case class Par(var name: String, stype: Stype) extends Paren {
	def get_name: String = name
}

final case object EmptyPar extends Paren {
	def get_name: String = ""
}

sealed trait Prog {
	val decl_list: ListBuffer[Decl]
}

final case class Program(val decl_list: ListBuffer[Decl]) extends Prog {
	override def toString: String = {
		decl_list.foldLeft("")((xs,x) => xs + x.toString)
	}
}

sealed trait Stype {
	val length: Int
	val stype: Stype
}

final case object Svoid extends Stype {
	override def toString: String = "void"
	val length = 0;
	val stype = Svoid;
}

final case object Sinteger extends Stype {
	override def toString: String = "int"
	val length = 0;
	val stype = Sinteger;
}

final case object Sbool extends Stype {
	override def toString: String = "bool"
	val length = 0;
	val stype = Sbool;
}
