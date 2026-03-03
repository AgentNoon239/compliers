package cdim
/*
This is the code generator for Keiko code
*/
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import cdim.error._
import cdim.token._
import cdim.ast._

class CodeGen(var tree: Prog) {

	/* String for header of code added to code at initialisation */
	private val code = new StringBuilder("MODULE Main 0 0\nIMPORT Lib 0\nENDHDR\n\n")

	/* The main public method that produces the Keiko code */
	def codeGen(): String = {
		var footer = ""
		var i = 0
		while (i < tree.decl_list.length) {
			tree.decl_list(i) match {
				case VarDecl(name, stype, line) => {
					footer = footer + "GLOVAR _" + name + " 4\n"
				}
				case FunDecl(name, param_list, block, _) => {
					genFunction(name, param_list, block)
				}
			}
			i += 1
		}
		(code ++= footer)
		code.toString
	}

	private def genFunction(name: String, param_list: ListBuffer[Paren], block: Stmt): Unit = {
		/* The number of local variables to put in function header */
		var locals = 0
		var offsets = offsetParam(param_list)
		block match {
			case BlockStmt(decls_list, stmt) => {
				if (name == "main") {
					(code ++= s"FUNC MAIN ${locals}\n")
				} else {
					(code ++= s"FUNC _${name} ${locals}\n")
				}
				genStmt(stmt, offsets)
				(code ++= "RETURN\n\n")
				(code ++= "END\n\n")
			}
			case x => {
				if (name == "main") {
					(code ++= s"FUNC MAIN 0\n")
				} else {
					(code ++= s"FUNC _${name} 0\n")
				}
				genStmt(x, offsets)
				(code ++= "RETURN\n\n")
				(code ++= "END\n\n")
			}
		}
	}

	/* Calculates offsets for parameters and stores in hash table */
	private def offsetParam(param_list: ListBuffer[Paren]): HashMap[String, Int] = {
		var offsets = HashMap[String, Int]()
		// Complete function
		offsets
	}

	/* Will generate Keiko code for functions, offsets should store offsets for parameters and local vars */
	protected def genStmt(stmt: Stmt, offsets: HashMap[String, Int]): Unit = {
		stmt match {
			case WhileStmt(expr, stmt) => {
				val lab1 = genLabel()
				val lab2 = genLabel()
				val lab3 = genLabel()
				jumpPrint(lab2)
				labelPrint(lab1)
				genStmt(stmt, offsets)
				labelPrint(lab2)
				genExpr(expr, lab1, lab3, offsets)
				labelPrint(lab3)
			}
			case IfStmt(expr, thenStmt, elseStmt) => {
				val lab1 = genLabel()
				val lab2 = genLabel()
				val lab3 = genLabel()
				genExpr(expr, lab1, lab2, offsets)
				labelPrint(lab1)
				genStmt(thenStmt, offsets)
				jumpPrint(lab3)
				labelPrint(lab2)
				genStmt(elseStmt, offsets)
				labelPrint(lab3)
			}
			case PrintStmt(expr) => {
				genExpr(expr, offsets)
				(code ++= "CONST 0\nGLOBAL lib.print\nPCALL 1\nCONST 0\nGLOBAL lib.newline\nPCALL 0\n")
			}
			case AssignStmt(expr1, expr2) => {
				expr1 match {
					case Variable(varName, stype, line) => {
						genExpr(expr2, offsets)
						offsets.get(varName) match {
							case None => (code ++= "STGW _" + varName + "\n")
							case Some (x) => (code ++= "STLW " + x.toString + "\n")
						}
					}
					case _ => ()
				}
			}
			case ReturnStmt(expr) => {
				genExpr(expr, offsets)
				(code ++= "RETURN\n")
			}
			case SeqStmt(list) => {
				for (stmt <- list) genStmt(stmt, offsets)
			}
			case _ => ()
		}
	}

	/* Expressions within functions */
	protected def genExpr(expr: Expr, offsets: HashMap[String, Int]): Unit = {
		expr match {
			case Num(value, _, _) => (code ++= ("CONST " + value + "\n"))
			case Variable(name, _, _) => {
				offsets.get(name) match {
					case None => (code ++= ("LDGW _" + name + "\n"))
					case Some (x) => (code ++= "LDLW " + x.toString + "\n")
				}
			}
			case UnaryExpr(op, expr, _, _) => {
				genExpr(expr, offsets)
				(code ++= op.keiko + "\n")
			}
			case BinaryExpr(expr1, op, expr2, _, _) => {
				genExpr(expr1, offsets)
				genExpr(expr2, offsets)
				(code ++= op.keiko + "\n")
			}
			case BoolV(0, _, _) => (code ++= "CONST 0\n")
			case BoolV(_, _, _) => (code ++= "CONST 1\n")
			case Nil(_,_) => ()
			case FunCall(name, arg_list, stype, line) => {
				var i = arg_list.length - 1
				while (i >= 0) {
					arg_list(i) match {
						case Nil(_, _) => ()
						case _ => genExpr(arg_list(i), offsets)
					}
					i -= 1
				}
				(code ++= s"CONST 0\nGLOBAL _${name}\nPCALLW ${numArgs(arg_list)}\n")
			}
		}
	}

	/* Conditional expressions within functions */
	protected def genExpr(expr: Expr, tlab: String, flab: String, offsets: HashMap[String, Int]): Unit = {
		expr match {
			case Num(value, _, _) => {
				if (value != 0) {
					jumpPrint(tlab)
				} else {
					jumpPrint(flab)
				}
			}
			case UnaryExpr(NOT, expr, _, _) => {
				genExpr(expr, flab, tlab, offsets)
			}
			case BinaryExpr(expr1, op, expr2, _, _) if op.relop => {
				genExpr(expr1, offsets)
				genExpr(expr2, offsets)
				jumpPrint(op.keiko, tlab)
				jumpPrint(flab)
			}
			case BinaryExpr(expr1, AND, expr2, _, _) => {
				val lab = genLabel()
				genExpr(expr1, lab, flab, offsets)
				labelPrint(lab)
				genExpr(expr2, tlab, flab, offsets)
			}
			case BinaryExpr(expr1, OR, expr2, _, _) => {
				val lab = genLabel()
				genExpr(expr1, tlab, lab, offsets)
				labelPrint(lab)
				genExpr(expr2, tlab, flab, offsets)
			}
			case BoolV(0, _, _) => jumpPrint(flab)
			case BoolV(_, _, _) => jumpPrint(tlab)
			case _ => {
				genExpr(expr, offsets)
				(code ++= "CONST 0\n")
				jumpPrint("NEQ", tlab)
				jumpPrint(flab)
			}
		}
	}

	/* Useful for call of function with PCALLW to indicate number of arguments */
	private def numArgs(arg_list: ListBuffer[Expr]): Int = {
		arg_list(0) match {
			case Nil(_, _) => 0
			case _ => arg_list.length
		}
	}

	/* Mutable integer tracking labels */
	private var label = 0;

	/* This generates labels for conditional operations */
	private def genLabel(): String = {label = label + 1; label.toString}

	/* Helper methods that add labels and jumps to the Stringbuilder object code */
	private def labelPrint(lab: String): Unit = (code ++= ("LABEL L" + lab + "\n"))
	protected def jumpPrint(lab: String): Unit = (code ++= ("JUMP L" + lab + "\n"))
	protected def jumpPrint(kop: String, lab: String): Unit = (code ++= ("J" + kop + " L" + lab + "\n"))
}
