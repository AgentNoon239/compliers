package cdim

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import cdim.ast._
import cdim.token._
import cdim.error._

class Semantic (val prog: Prog) {

	/* Public method that performs semantic check on AST and returns updated AST */
	def check(): Prog = {
		var env0 = HashMap[String, ListBuffer[Stype]]()
		var i = 0
		while (i < prog.decl_list.length) {
			env0 = checkDecl(prog.decl_list(i), env0)
			i += 1
		}
		if (!isMain) throw ParserException("There is no function called main declared")
		prog
	}

	private var isMain = false

	/* Symbol consists of two HashMaps for identifiers and function output and function input types as a list */
	private type env = HashMap[String, ListBuffer[Stype]]

	private def checkDecl(decl: Decl, env0: env): env = {
		var env_init = env0
		val argTypes = ListBuffer[Stype]()
		decl match {
			case VarDecl(name, stype, line) => {
				// Add declaration to global symbol table env
				notInEnv(name, env_init, line)
				argTypes += stype
				env_init = env_init + (name -> argTypes)
			}
			case FunDecl(name, param_list, block, line) => {
				// Check if already declared
				notInEnv(name, env_init, line)
				// Create copy of environment that will be updated by parameters
				var env_new = env0
				// Create empty environment to check for redeclarations
				var env_check = HashMap[String, ListBuffer[Stype]]()
				// For creating part of symbol table storing input types - first it stores output type
				argTypes += Sinteger
				param_list.foreach {
					case Par(pName, pStype) => {
						// Check that pName is not in local declarations
						notInEnv(pName, env_check, line)
						val pTypeList = ListBuffer[Stype]()
						pTypeList += pStype
						// Add to new copy of local symbol table
						env_new = env_new + (pName -> pTypeList)
						// If not already declared add to env_check
						env_check = env_check + (pName -> pTypeList)
						// Add input types
						argTypes += pStype
					}
					case EmptyPar => argTypes += Svoid
				}
				// Checks that there is a main function
				if (name == "main") {
					isMain = true
					if (argTypes(1) != Svoid) throw ParserException("Function main should have no parameters")
				}
				// Add function declaration to environments as long as it's not in the params
				env_init = env_init + (name -> argTypes)
				env_new.get(name) match {
					case None =>  {
						env_new = env_new + (name -> argTypes)
					}
					case _ => ()
				}
				checkBody(block, env_new, env_check)
				if (!checkReturn(block)) {
					throw new ParserException("Missing return in function "+name + " declared on line " + line)
				}
			}
		}
		env_init
	}

	private def checkBody(stmt: Stmt, env_new: env, env_check: env): Unit = {
		var env_c = env_check
		var env_n = env_new
		stmt match {
			case BlockStmt(decl_list, bStmt) => {
				decl_list.foreach {
					case VarDecl(name, stype, line) => {
						notInEnv(name, env_c, line)
						val sList = ListBuffer[Stype]()
						sList += stype
						env_c = env_c + (name -> sList)
						env_n = env_n + (name -> sList)
					}
					case FunDecl(name, _, _, line) => throw ParserException("Forbidden declaration of function " + name + " on line " + line + " within a function")
				}
				checkStmt(bStmt, env_n)
			}
			case _ => {
				checkStmt(stmt, env_n)
			}	
		}
		()
	}

	/* Checks statements instance by instance */
	private def checkReturn(stmt: Stmt): Boolean = {
		stmt match {
			case IfStmt(expr, thenStmt, elseStmt) => {
				checkReturn(thenStmt) && checkReturn(elseStmt)
			}
			case SeqStmt(stmt_list) => stmt_list.exists(checkReturn)
			case BlockStmt(decl_list, bStmt) => checkReturn(bStmt)
			case ReturnStmt(expr) => true
			case _ => false
		}
	}

	/* Checks statements instance by instance */
	private def checkStmt(stmt: Stmt, env0: env): Unit = {
		stmt match {
			case PrintStmt(expr) => checkExpr(expr, env0)
			case WhileStmt(expr, stmt) => {
				checkExpr(expr, env0)
				checkType(expr, Sbool)
				checkStmt(stmt, env0)
			}
			case IfStmt(expr, thenStmt, elseStmt)  => {
				checkExpr(expr, env0)
				checkType(expr, Sbool)
				checkStmt(thenStmt, env0)
				checkStmt(elseStmt, env0)
			}
			case AssignStmt(expr1, expr2) => {
				checkExpr(expr1, env0)
				checkExpr(expr2, env0)
				checkType(expr1, expr2)
			}
			case SeqStmt(stmt_list) => for (stmt <- stmt_list) checkStmt(stmt, env0)
			case ReturnStmt(expr) => {
				checkExpr(expr, env0)
				checkType(expr, Sinteger)
			}
			// There should be no BlockStmt within BlockStmt - it would not be parsed
			case _ => ()
		}
	}

	/* Checks expressions instance by instance */
	private def checkExpr(expr: Expr, env0: env): Unit = {
		expr match {
			case BinaryExpr(expr1, op, expr2, stype, line) => {
				checkExpr(expr1, env0)
				checkExpr(expr2, env0)
				/* relop is true if the operation corresponding to op: Op is relational, and false otherwise */
				if (op.relop) checkType(expr1, expr2) else checkType(expr1, expr2, stype)
			}
			case UnaryExpr(op, expr, stype, line) => {
				checkExpr(expr, env0)
				checkType(expr, stype)
			}
			case Variable(name, stype, line) => {
				expr.stype = inEnv(name, env0, line)
				val varType = env0.get(name).get
				if (varType.length > 1) throw ParserException("Error on line " + line + ": " + name + " is not a variable, but a function")
			}
			case Num(_, _, _) => ()
			case BoolV(_, _, _) => ()
			case Nil(_, _) => ()
			case FunCall(name, arg_list, stype, line) => {
				inEnv(name, env0, line)
				// Checks if it is a function
				val funTypes = env0.get(name).get
				if (funTypes.length < 2) throw ParserException("Error on line " + line + ": " + name + " is not a function")
				// Gets the input types and checks have the right number
				var param_list = funTypes.tail
				if (arg_list.length != param_list.length) {
					throw ParserException("Error on line " + line +": function " + name + " needs exactly " + param_list.length.toString + " arguments")
				}
				// Checks expression is semantically valid and the list of parameters has correct type
				var i = 0;
				if (param_list(0) == Svoid) {
						if (arg_list(0).stype != Svoid) throw ParserException("Error on line " + line +": function " + name + " expects no parameters")
				} else {
					while (i < param_list.length) {
						checkExpr(arg_list(i), env0)
						checkType(arg_list(i), param_list(i))
						i += 1;
					}
				}
			}
		}
	}

	/* Helper functions - first we overload a type checker */
		var offsets = HashMap[String, Int]()
		var offset = 16
		// Complete function
		param_list.foreach(i => {
			i match {
				case Par(name, stype) => {
					offsets += (name -> offset)
					offset += 4
				}
				case EmptyPar => {}
			}
		})
		offsets
	/* This will check two operands have same type -- this is useful for relational operators */
	protected def checkType(expr1: Expr, expr2: Expr): Unit = {
		if ((expr1.stype == expr2.stype)) () else throw ParserException("Type mismatch with expression on line " + expr1.line)
	}

	/* Checks if two operands have same type and it's equal to parameter stype -- useful for binary arithmetic in bool and int */
	protected def checkType(expr1: Expr, expr2: Expr, stype: Stype): Unit = {
		if ((expr1.stype == expr2.stype) && (expr1.stype == stype)) () else throw ParserException("Type mismatch with expression on line " + expr1.line + " expected " + stype)
	}

	/* This will check operand has same type as parameter stype -- this is useful for unary operators*/
	protected def checkType(expr: Expr, stype: Stype): Unit = {
		if ((expr.stype == stype)) () else throw ParserException("Type mismatch with expression on line " + expr.line + " expected " + stype)
	}

	/* Two functions to check whether a value is in the environment and are used in different contexts */
	/* inEnv returns the Stype value so we can put that value into the annotated AST */
	private def inEnv(name: String, env0: HashMap[String, ListBuffer[Stype]], line: Int): Stype = {
		env0.get(name) match {
			case None => throw ParserException("Undeclared identifier " + name + " on line " + line)
			case Some(_) => env0.get(name).get(0)
		}
	}

	/* Used for checking if identifier has already been declared */
	private def notInEnv(name: String, env0: HashMap[String, ListBuffer[Stype]], line: Int): Unit = {
		env0.get(name) match {
			case None => ()
			case Some(_) => throw ParserException("Error on line " + line + ": identifier " + name + " has already been declared")
		}
	}

}