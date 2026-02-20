package cdim

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import cdim.ast._
import cdim.token._
import cdim.error._

class Semantic (val prog: Prog) {
	
	/* Symbol table stored as a mutable HashMap */
	private val env = HashMap[String, Stype]()

	/* Public method that performs semantic check on annotated AST and returns updated annotated AST */
	def check(): Prog = {
		prog.decl_list.foreach {
			case Declr(name, stype, line) => {notInEnv(name, env, line); env+=(name -> stype)}
		}
		for (stmt <- prog.stmt_list) checkStmt(stmt, env)
		prog
	}

	/* Checks statements */
	private def checkStmt(stmt: Stmt, env: HashMap[String, Stype]): Unit = {
		stmt match {
			case PrintStmt(expr) => checkExpr(expr, env)
			case WhileStmt(expr, stmt) => {
				checkExpr(expr, env)
				checkType(expr, Sbool)
				checkStmt(stmt, env)
			}
			case IfStmt(expr, thenStmt, elseStmt)  => {
				checkExpr(expr, env)
				checkType(expr, Sbool)
				checkStmt(thenStmt, env)
				checkStmt(elseStmt, env)
			}
			case AssignStmt(expr1, expr2) => {
				checkExpr(expr1, env)
				checkExpr(expr2, env)
				
				checkType(expr1, expr2)
			}
			case SeqStmt(stmt_list) => {
				stmt_list.foreach(i => checkStmt(i, env))
			}
			case BlockStmt(decl_list, stmt) => {
				val innerHmap = new HashMap[String, Stype]()
				decl_list.foreach{
					case Declr(name, stype, line) => {notInEnv(name, innerHmap, line); innerHmap+=(name -> stype)}
				}
				checkStmt(stmt, env.concat(innerHmap))
			}
			case EmptyStmt => ()
		}
	}

	/* Checks expressions */
	private final def checkExpr(expr: Expr, env: HashMap[String, Stype]): Unit = {
		expr match {
			case BinaryExpr(expr1, op, expr2, stype, line) => {
				checkExpr(expr1, env)
				checkExpr(expr2, env)
				/* op.relop is true if the operation op is relational, and false otherwise */
				if (op.relop) checkType(expr1, expr2) else checkType(expr1, expr2, stype)
			}
			case UnaryExpr(op, expr, stype, line) => {
				checkExpr(expr, env)
				checkType(expr, stype)
			}
			case Variable(name, stype, line) => {
				var new_stype = inEnv(name, env, line)
				if (!isPrimitive(new_stype)) {
					throw new ParserException("Variable does not have basic type on line "+line)
				}
				expr.stype = new_stype
			}
			case ArrayElement(name, expr_list, stype, line) => {
				expr.stype = checkArray(expr_list,env(name),line)
			}
			case Num(value, stype, line) => ()
			case BoolV(value, stype, line) => ()
		}
	}

	/* Helper functions - first we overload a type checker */

	/* This will check two operands have same type -- this is useful for relational operators */
	protected def checkType(expr1: Expr, expr2: Expr): Unit = {
		if ((expr1.stype == expr2.stype)) () else throw ParserException("Type mismatch with expression on line " + expr1.line)
	} _ => throw ParserException("Index is not integer on line "+line)
				}
				
			}
			case _ => {
				if (!expr_list.isEmpty) {
					

	/* Checks if two operands have same type and it's equal to parameter stype -- useful for binary arithmetic in bool and int */
	protected def checkType(expr1: Expr, expr2: Expr, stype: Stype): Unit = {
		if ((expr1.stype == expr2.stype) && (expr1.stype == stype)) () else throw ParserException("Type mismatch with expression on line " + expr1.line + " expected a " + stype)
	}

	/* This will check operand has same type as parameter stype -- this is useful for unary operators*/
	protected def checkType(expr: Expr, stype: Stype): Unit = {
		if ((expr.stype == stype)) () else throw ParserException("Type mismatch with expression on line " + expr.line + " expected a " + stype)
	}

	/* Two functions to check whether a value is in the environment and are used in different contexts */
	/* inEnv returns the Stype value so we can put that value into the annotated AST */
	private final def inEnv(name: String, env: HashMap[String, Stype], line: Int): Stype = {
		env.get(name) match {
			case None => throw ParserException("Undeclared variable " + name + " on line " + line)
			case Some(_) => env.get(name).get
		}
	}

	/* Used for checking if variable has already been declared */
	private final def notInEnv(name: String, env: HashMap[String, Stype], line: Int): Unit = {
		env.get(name) match {
			case None => ()
			case Some(_) => throw ParserException("Error on line " + line + ": variable " + name + " has already been declared")
		}
	}

	private def isPrimitive(stype: Stype): Boolean = {
		stype match {
			case Sbool => true
			case Sinteger => true
			case Svoid => true
			case _ => false
		}
	}

	private def checkArray(expr_list: ListBuffer[Expr], stype: Stype, line: Int): Stype = {
		stype match {
			case Sarray(length, stype2) => {
				if (expr_list.isEmpty) {
					throw ParserException("Too few indices for array on line "+line)
				}
				expr_list.head.stype match {
					case Sinteger => checkArray(expr_list.tail, stype2,line)
					case _ => throw ParserException("Index is not integer on line "+line)
				}
				
			}
			case _ => {
				if (!expr_list.isEmpty) {
					throw ParserException("Too many indices for array on line "+line)
				}
				return stype
			}
		}

	}
}