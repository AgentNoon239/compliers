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

	private variables: List[HashMap] = new List()
	private treeStack: Stack = new Stack()

	def execute = {

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
			case UnaryExpr(expr1, UMINUS) => {
				-executeExpr(expr1)
			}
			case UnaryExpr(expr1, NOT) => {
				!executeExpr(expr1)
			}
			case Variable(name: String) => {

			}
		}
	}

	private def resolveVar(name: String) = {
		while (true) {
			val h = variables.headOption
			scope = 
		}
	}




}