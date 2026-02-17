package cdim
/*
This is the parser takes the lexer as a parameter and has only one public method: parse()

It is a recursive descent parser that will call the lexer iterator methods when it needs new tokens 
*/
import scala.collection.mutable.HashMap

import cdim.ast._
import cdim.token._
import cdim.error._

class Interpreter (val prog: Program) {

	private var variables: HashMap[String, Int] = new HashMap[String, Int]()

	def execute = {
		for (stmt <- prog.list) {
			executeStmt(stmt)
		}
	}

	private def executeStmt(stmt: Stmt): Unit = {
		stmt match {
			case AssignStmt(name, expr) => {
				variables(name) = executeExpr(expr)
			}
			case SeqStmt(stmts) => {
				for (stmt <- stmts) {
					executeStmt(stmt)
				}
			}
			case IfStmt(expr, thenStmt, elseStmt) => {
				if (executeExpr(expr) != 0) {
					executeStmt(thenStmt)
				} else {
					executeStmt(elseStmt)
				}
			}
			case WhileStmt(expr, stmt) => {
				while (executeExpr(expr) != 0) {
					executeStmt(stmt)
				}
			}
			case PrintStmt(expr) => {
				println(executeExpr(expr))
			}
			case EmptyStmt => {}
		}
	}

	private def executeExpr(expr: Expr): Int = {
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
			case BinaryExpr(exp1, EQUAL_EQUAL, expr2) => {
				if (executeExpr(exp1) == executeExpr(expr2)) 1 else 0
			}
			case BinaryExpr(exp1, BANG_EQUAL, expr2) => {
				if (executeExpr(exp1) != executeExpr(expr2)) 1 else 0
			}
			case BinaryExpr(exp1, LESS, expr2) => {
				if (executeExpr(exp1) < executeExpr(expr2)) 1 else 0
			}
			case BinaryExpr(exp1, GREATER, expr2) => {
				if (executeExpr(exp1) > executeExpr(expr2)) 1 else 0
			}
			case BinaryExpr(exp1, LESS_EQUAL, expr2) => {
				if (executeExpr(exp1) <= executeExpr(expr2)) 1 else 0
			}
			case BinaryExpr(exp1, GREATER_EQUAL, expr2) => {
				if (executeExpr(exp1) >= executeExpr(expr2)) 1 else 0
			}
			case BinaryExpr(exp1, AND, expr2) => {
				executeExpr(exp1) & executeExpr(expr2)
			}
			case BinaryExpr(exp1, OR, expr2) => {
				executeExpr(exp1) | executeExpr(expr2)
			}
			case UnaryExpr(UMINUS,expr1) => {
				-executeExpr(expr1)
			}
			case UnaryExpr(NOT,expr1) => {
				if (executeExpr(expr1) == 0) 1 else 0
			}
			case Variable(name: String) => variables.getOrElse(name, throw new RuntimeException("Variable " + name + " not defined"))
			case Num(value) => value
			case TernaryExpr(exprcond, exprt, exprf) => {
				if (executeExpr(exprcond) != 0) {
					executeExpr(exprt)
				} else {
					executeExpr(exprf)
				}
			}
			case _ => throw new RuntimeException("Invalid expression")
		}
	}
}