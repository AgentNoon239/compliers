package cdim
/*
This is the code generator for Keiko code
*/
import scala.collection.immutable.HashMap
import scala.collection.immutable.Set
import scala.collection.mutable.ListBuffer
import cdim.token._
import cdim.ast._
import cdim.error._

class CodeGen(var tree: Prog) {

	/* MAIN is the entry point for procedures in Keiko code */
	/* String for header of code added to code at initialisation */
	private val code = new StringBuilder("MODULE Main 0 0\nIMPORT Lib 0\nENDHDR\nFUNC MAIN 0\n\n")
	private var str = "";

	/* This function produces both the symbol table and a string for the Keiko code footer in env and str respectively */
	private def genSyms(declList: ListBuffer[Decl],env0: HashMap[String, Stype], aliasTable0: HashMap[String, String]): (HashMap[String, Stype], HashMap[String, String]) = {
		var aliasTable = aliasTable0
		var env = env0
		declList.foreach {
			case Declr(name, stype, line) => {
				var lName = findVarSpace(name, aliasTable)
				aliasTable += (name -> lName)
				env = env + (lName -> stype)
				str = str + "GLOVAR _" + lName + s" ${calcWidth(stype)}\n"
			}
		}
		(env, aliasTable)
	}

	private def findVarSpace(name: String, aliasTable: HashMap[String, String]): String = {
		if (aliasTable.contains(name)) {
			findVarSpace(name + "0", aliasTable)
		} else {
			return name
		}
	}

	/* This initialises the symbol table and generates the footer string for the code */
	private var (env, aliasTable): (HashMap[String, Stype], HashMap[String, String]) = genSyms(tree.decl_list, HashMap[String, Stype](), HashMap[String, String]())

	/* The main public method that produces the Keiko code as a Stringbuilder object */
	def codeGen(): String = {
		for (stmt <- tree.stmt_list) genStmt(stmt, env, aliasTable)
		(code ++= "RETURN\n")
		(code ++= "END\n\n")
		(code ++= str)
		code.toString
	}

	/* Stores Keiko code strings as a side effect in code by pattern matching on Stmt objects */
	private def genStmt(stmt: Stmt, envLocal: HashMap[String, Stype], aliasTable: HashMap[String, String]): Unit = {
		stmt match {
			case WhileStmt(expr, stmt) => {
				var lab1 = genLabel()
				var lab2 = genLabel()
				var lab3 = genLabel()
				labelPrint(lab1)
				genExpr(expr,lab2,lab3, envLocal, aliasTable)
				labelPrint(lab2)
				genStmt(stmt, envLocal, aliasTable)
				jumpPrint(lab1)
				labelPrint(lab3)
			}
			case BlockStmt(decl_list, stmt) => {
				// Loop throw new decls
				// Make new variables for clash
				// Pass list of aliases further down
				var (env2, aliasTable2): (HashMap[String, Stype], HashMap[String, String]) = genSyms(decl_list,envLocal,aliasTable)
				genStmt(stmt, env2, aliasTable2)
			}
			case IfStmt(expr, thenStmt, elseStmt) => {
				val lab1 = genLabel()
				val lab2 = genLabel()
				val lab3 = genLabel()
				genExpr(expr, lab1, lab2, envLocal, aliasTable)
				labelPrint(lab1)
				genStmt(thenStmt, envLocal, aliasTable)
				jumpPrint(lab3)
				labelPrint(lab2)
				genStmt(elseStmt, envLocal, aliasTable)
				labelPrint(lab3)
			}
			case EmptyStmt => ()
			case PrintStmt(expr) => {
				genExpr(expr, envLocal, aliasTable)
				(code ++= "CONST 0\nGLOBAL lib.print\nPCALL 1\nCONST 0\nGLOBAL lib.newline\nPCALL 0\n")
			}
			case AssignStmt(expr1, expr2) => {
				expr1 match {
					case Variable(varName, stype, line) => {genExpr(expr2, env, aliasTable); (code ++= "STGW _" + aliasTable(varName) + "\n")}
					case ArrayElement(arrName, expr_list, stype, line) => {
						genExpr(expr2, envLocal, aliasTable);
						code ++= "GLOBAL _" + aliasTable(arrName) + "\n"
						env(arrName) match {
							case Sarray(_,stype2) => {
								code ++= "CONST 0\n"
								ravel(expr_list, stype2, env, aliasTable)
							}
							case _ => {
								throw new ParserException("ArrayElement does not have type array on line "+line)
							}
						}
						code ++= "OFFSET\nSTOREW\n"
					}
					case _ => ()
				}
			}
			case SeqStmt(list) => {
				for (stmt <- list) genStmt(stmt, envLocal, aliasTable)
			}
		}
	}


	// Index i[a][b] at [x][y] = b*x + y
	// Index i[a][b][c] at [x][y][z] = b*c*x + c*y + z


	protected def ravel(expr_list: ListBuffer[Expr], stype: Stype, env: HashMap[String, Stype], aliasTable: HashMap[String, String]): Unit = {
		genExpr(expr_list.head, env, aliasTable)
		code ++= "PLUS\n"
		stype match {
			case Sarray(length,stype2) => {
				code ++= "CONST " + length + "\n"
				code ++= "TIMES\n"
				ravel(expr_list.tail, stype2, env, aliasTable)
			}
			case _ => {
				code ++= "CONST 4\n"
				code ++= "TIMES\n"
			}
		}
		
	}

	/* This method is for expressions when they are not conditions */
	protected def genExpr(expr: Expr, env: HashMap[String, Stype], aliasTable: HashMap[String, String]): Unit = {
		expr match {
			case Num(value, _, _) => (code ++= ("CONST " + value + "\n"))
			case Variable(name, _, _) => (code ++= ("LDGW _" + aliasTable(name) + "\n"))
			case UnaryExpr(op, expr, _, _) => {
				genExpr(expr, env, aliasTable)
				(code ++= op.keiko + "\n")
			}
			case BinaryExpr(expr1, op, expr2, _, _) => {
				genExpr(expr1, env, aliasTable)
				genExpr(expr2, env, aliasTable)
				(code ++= op.keiko + "\n")
			}
			case ArrayElement(name, expr_list, stype, line) => {
				code ++= "GLOBAL _" + aliasTable(name) + "\n"
				env(name) match {
					case Sarray(_,stype2) => {
						code ++= "CONST 0\n"
						ravel(expr_list, stype2, env, aliasTable)
					}
					case _ => {
						throw new ParserException("ArrayElement does not have type array on line "+line)
					}
				}
				code ++= "OFFSET\nLOADW\n"
			}
			case BoolV(0, _, _) => (code ++= "CONST 0\n")
			case BoolV(_, _, _) => (code ++= "CONST 1\n")
		}
	}

	/* This method is for condition expressions - when they are in the condition of if or while */
	protected def genExpr(expr: Expr, tlab: String, flab: String, env: HashMap[String, Stype], aliasTable: HashMap[String, String]): Unit = {
		expr match {
			case Num(value, _, _) => {
				if (value != 0) {
					jumpPrint(tlab)
				} else {
					jumpPrint(flab)
				}
			}
			case UnaryExpr(NOT, expr, _, _) => {
				genExpr(expr, flab, tlab, env, aliasTable)
			}
			case BinaryExpr(expr1, op, expr2, _, _) if op.relop => {
				genExpr(expr1, env, aliasTable)
				genExpr(expr2, env, aliasTable)
				jumpPrint(op.keiko, tlab)
				jumpPrint(flab)
			}
			case BinaryExpr(expr1, AND, expr2, _, _) => {
				val lab = genLabel()
				genExpr(expr1, lab, flab, env, aliasTable)
				labelPrint(lab)
				genExpr(expr2, tlab, flab, env, aliasTable)
			}
			case BinaryExpr(expr1, OR, expr2, _, _) => {
				val lab = genLabel()
				genExpr(expr1, tlab, lab, env, aliasTable)
				labelPrint(lab)
				genExpr(expr2, tlab, flab, env, aliasTable)
			}
			case BoolV(0, _, _) => jumpPrint(flab)
			case BoolV(_, _, _) => jumpPrint(tlab)
			case _ => {
				genExpr(expr, env, aliasTable)
				(code ++= "CONST 0\n")
				jumpPrint("NEQ", tlab)
				jumpPrint(flab)
			}
		}
	}

	/* Useful for the footer generator - computes the width of a data type */
	private def calcWidth(stype: Stype): Int = {
		stype match {
			case Sarray(num, stype1) => num * calcWidth(stype1)
			case Sinteger => 4
			case Sbool => 4
			case _ => 0
		}
	}

	/* Mutable integer tracking labels */
	private var label = 1;

	/* This generates labels for conditional operations */
	private def genLabel(): String = {label = label + 1; label.toString}

	/* Helper methods that add labels and jumps to the Stringbuilder object code */
	private def labelPrint(lab: String): Unit = (code ++= ("LABEL L" + lab + "\n"))
	protected def jumpPrint(lab: String): Unit = (code ++= ("JUMP L" + lab + "\n"))
	protected def jumpPrint(kop: String, lab: String): Unit = (code ++= ("J" + kop + " L" + lab + "\n"))
}
