package cdim
/*
This is the parser takes the lexer as a parameter and has only one public method: parse()

It is a recursive descent parser that will call the lexer iterator methods when it needs new tokens 
*/
import scala.collection.mutable.Stack,  scala.collection.mutable.HashMap, scala.collection.mutable.List

import cdim.ast._
import cdim.token._
import cdim.error._

class Interpreter (val prog: Prog) {

	private variables: new HashMap[String, Int] = new HashMap[String, Int]()

	def execute = {
		for (stmt <- prog.stmts) {
			executeStmt(stmt)
		}
	}

	private def executeStmt(stmt: Stmt) = {
		stmt match {
			AssignStmt(name, expr) => {
				variables(name) = executeExpr(expr)
			}
			SeqStmt(stmts) => {
				for (stmt <- stmts) {
					executeStmt(stmt)
				}
			}
			IfStmt(expr, thenStmt, elseStmt) => {
				if (executeExpr(expr)) {
					executeStmt(thenStmt)
				} else {
					executeStmt(elseStmt)
				}
			}
			WhileStmt(expr, stmt) => {
				while (executeExpr(expr)) {
					executeStmt(stmt)
				}
			}
		}

	private def executeExpr(expr: Expr) = {
		expr match {
			case BinaryExpr(expr1, ADD, expr2) => {
				executeExpr(expr1) + executeExpr(expr2)
			}
			case BinaryExpr(expr1, SUBTRACT, expr2) => {
				executeExpr(expr1) - executeExpr(expr2)
			}
			case BinaryExpr(expr1, TIMES, expr2) => {
				executeExpr(expr1) * executeExpr(expr2)
			}
			case BinaryExpr(expr1, DIV, expr2) => {
				executeExpr(expr1) / executeExpr(expr2)
			}
			case BinaryExpr(exp1, EQ, expr2) => {
				executeExpr(exp1) == executeExpr(expr2)
			}
			case BinaryExpr(exp1, NEQ, expr2) => {
				executeExpr(exp1) != executeExpr(expr2)
			}
			case BinaryExpr(exp1, LT, expr2) => {
				executeExpr(exp1) < executeExpr(expr2)
			}
			case BinaryExpr(exp1, GT, expr2) => {
				executeExpr(exp1) > executeExpr(expr2)
			}
			case BinaryExpr(exp1, LTE, expr2) => {
				executeExpr(exp1) <= executeExpr(expr2)
			}
			case BinaryExpr(exp1, GTE, expr2) => {
				executeExpr(exp1) >= executeExpr(expr2)
			}
			case BinaryExpr(exp1, AND, expr2) => {
				executeExpr(exp1) && executeExpr(expr2)
			}
			case BinaryExpr(exp1, OR, expr2) => {
				executeExpr(exp1) || executeExpr(expr2)
			}
			case UnaryExpr(expr1, UMINUS) => {
				-executeExpr(expr1)
			}
			case UnaryExpr(expr1, NOT) => {
				!executeExpr(expr1)
			}
			case Variable(name: String) => {
				variables.get(name) match {
					case Some(value) => value
					case None => throw new RuntimeError("Variable " + name + " not defined")
				}
			}
			case Num(value) => {
				value
			}
			case TernaryExpr(exprcond, exprt, exprf) => {
				if (executeExpr(exprcond)) {
					executeExpr(exprt)
				} else {
					executeExpr(exprf)
				}
			}
		}
	}
}