package cdim

import scala.collection.mutable.ListBuffer 

import cdim.ast._
import cdim.token._
import cdim.error._

class Parser (val tokens: Lexer) {

	def parse(): Prog = {
		Program(parseDecls())
	}

	private final var currentToken = if (tokens.hasNext) tokens.next() else throw NoSuchElementException

	private final def parseDecls(): ListBuffer[Decl] = {
		val decl_list = ListBuffer[Decl]()
		while(tokens.hasNext) {
			decl_list ++= parseDecl()
		}
		decl_list
	}

	private final def parseDecl(): ListBuffer[Decl] = {
		currentToken match {
			case TYPE(stype) => {
				advance()
				currentToken match {
					case IDENT(name) => {advance(); parseGlobal(name, stype)}
					case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected name")
				}
			}
			case FUNCTION => {
				advance()
				currentToken match {
					case IDENT(name) => {advance(); parseFun(name)}
					case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected name")
				}
			}
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected a declaration of a type")
		}
	}

	private final def parseGlobal(name: String, stype: Stype): ListBuffer[Decl] = {
		val list_decl = ListBuffer[Decl]()
		currentToken match {
			case COMMA => {
				advance()
				list_decl += VarDecl(name, stype, tokens.line)
				list_decl ++ parseNames(stype)
			}
			case SEMICOLON => {
				advance()
				list_decl += VarDecl(name, stype, tokens.line)
			 }
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected one of ',' or ';' after " + name)
		}
		list_decl
	}

	private final def parseFun(name: String): ListBuffer[Decl] = {
		val list_decl = ListBuffer[Decl]()
		currentToken match {
			case LEFT_PAREN => {
				advance()
				val line = tokens.line
				val params = parseParams()
				consume(LEFT_BRACE)
				val body = block()
				consume(RIGHT_BRACE)
				list_decl += FunDecl(name, params, body, line)
			}
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected '(' in declaration of " + name)
		}
		list_decl
	} 

	private final def parseParams(): ListBuffer[Paren] = {
		val param_list = ListBuffer[Paren]()
		param_list += parseParam()
		while (tokens.hasNext && (currentToken != RIGHT_PAREN)) {
			currentToken match {
				case COMMA => {advance(); param_list += parseParam()}
				case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected ',' in list of parameters")
			}
		}
		consume(RIGHT_PAREN)
		param_list
	}

	private final def parseParam(): Paren = {
		currentToken match {
			case TYPE(stype) => {
				advance()
				currentToken match {
					case IDENT(name) => {advance(); Par(name, stype)}
					case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected name of parameter")
				}
			}
			case RIGHT_PAREN => {
				EmptyPar
			}
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected parameter type")
		}
	}

	private final def block(): Stmt = {
		val decls_list = parseVars()
		val stmt_list = ListBuffer[Stmt]()
		while(tokens.hasNext && (currentToken != RIGHT_BRACE)) {
			stmt_list += parseStmt()
		}
		if (stmt_list.isEmpty) return EmptyStmt
		if (decls_list.isEmpty) {
			if (stmt_list.length == 1) stmt_list(0) else SeqStmt(stmt_list)
		} else {
			if (stmt_list.length == 1) {
				BlockStmt(decls_list, stmt_list(0))
			} else {
				BlockStmt(decls_list, SeqStmt(stmt_list))
			}
		}
	}

	private final def parseVars(): ListBuffer[Decl] = {
		var decl_list = ListBuffer[Decl]()
		var decl = parseVar()
		while (!decl.isEmpty) {
			consume(SEMICOLON)
			decl_list = decl_list ++ decl
			decl = parseVar()
		}
		decl_list
	}

	private final def parseVar(): ListBuffer[Decl] = {
		val list = ListBuffer[Decl]()
		currentToken match {
			case TYPE(stype) => {
				advance()
				val names = parseNames(stype)
				if (!names.isEmpty) {
					list ++ names
				} else throw ParserException("Syntax error on line " + tokens.line + ": expected variable name")
			}
			case _ => list
		}
	}

	private final def parseNames(stype: Stype): ListBuffer[Decl] = {
		val name_list = ListBuffer[Decl]()
		name_list += parseName(stype)
		while(tokens.hasNext && (currentToken != SEMICOLON)) {
			currentToken match {
				case COMMA => {advance(); name_list += parseName(stype)}
				case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected ',' in declaration")
			}
		}
		name_list
	}

	private final def parseName(stype: Stype): Decl = {
		currentToken match {
			case IDENT(name) => {
				advance()
				VarDecl(name, stype, tokens.line)
			}
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected variable name")
		}
	}

	private final def parseStmts(): ListBuffer[Stmt] = {
		val stmt_list = ListBuffer[Stmt]()
		while(tokens.hasNext) {
			stmt_list += parseStmt()
		}
		stmt_list
	}

	private final def parseStmt(): Stmt = {
		currentToken match {
			case LEFT_BRACE => {advance(); val stmts = parseCompoundStmt(); consume(RIGHT_BRACE); stmts}
			case PRINT => {advance(); parsePrint()}
			case IDENT(name) => {
				advance()
				val expr_left = Variable(name, Svoid, tokens.line)
				consume(EQUAL)
				val expr_right = parseOr()
				consume(SEMICOLON)
				AssignStmt(expr_left, expr_right)
			}
			case IF => {advance(); parseIf()}
			case WHILE => {advance(); parseWhile()}
			case SEMICOLON => {advance(); EmptyStmt}
			case RETURN => {advance(); parseReturn()}
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected a statement or ';'")
		}
	}

	private final def parseCompoundStmt(): Stmt = {
		val stmt_list = ListBuffer[Stmt]()
		while(tokens.hasNext && (currentToken != RIGHT_BRACE)) {
			stmt_list += parseStmt()
		}
		if (stmt_list.isEmpty) return EmptyStmt
		else if (stmt_list.length == 1) return stmt_list(0)
		else SeqStmt(stmt_list)
	}

	private final def parseReturn(): Stmt = {
		currentToken match {
			case SEMICOLON => throw ParserException("Syntax error on line " + tokens.line + ": need to return an integer")
			case _ => {
				val ret = ReturnStmt(calcExpr())
				consume(SEMICOLON)
				ret
			}
		}
	}

	private final def parsePrint(): Stmt = {
		consume(LEFT_PAREN)
		val e = parseOr()
		consume(RIGHT_PAREN)
		consume(SEMICOLON)
		PrintStmt(e)
	}

	private final def parseIf(): Stmt = {
		consume(LEFT_PAREN)
		val cond = parseOr()
		consume(RIGHT_PAREN)
		val thenst = parseStmt()
		currentToken match {
			case ELSE => {advance(); IfStmt(cond,thenst,parseStmt())}
			case _ => {IfStmt(cond,thenst,EmptyStmt)}
		}
	}

	private final def parseWhile(): Stmt = {
		consume(LEFT_PAREN)
		val cond = parseOr()
		consume(RIGHT_PAREN)
		WhileStmt(cond, parseStmt())
	}

	private final def parseOr(): Expr = {
		parseOr2(parseAnd())
	}

	private final def parseOr2(e: Expr): Expr = {
		currentToken match {
			case BINOP(OR) => {advance(); parseOr2(BinaryExpr(e, OR, parseAnd(), Sbool, tokens.line))}
			case _ => e
		}
	}

	private final def parseAnd(): Expr = {
		parseAnd2(expr())
	}

	private final def parseAnd2(e: Expr): Expr = {
		currentToken match {
			case BINOP(AND) => {advance(); parseAnd2(BinaryExpr(e, AND, expr(), Sbool, tokens.line))}
			case _ => e
		}
	}

	private final def expr(): Expr = {
		expr2(calcExpr())
	}

	private final def expr2(e: Expr): Expr = {
		currentToken match {
			case RELOP(op) => {advance(); expr2(BinaryExpr(e, op, calcExpr(), Sbool, tokens.line))}
			case _ => e
		}
	}

	private final def calcExpr(): Expr = {
		calcExpr2(term())
	}

	private final def calcExpr2(e: Expr): Expr = {
		currentToken match {
			case MINUS => {advance(); calcExpr2(BinaryExpr(e, SUBTRACT, term(), Sinteger, tokens.line))}
			case BINOP(ADD) => {advance(); calcExpr2(BinaryExpr(e, ADD, term(), Sinteger, tokens.line))}
			case _ => e
		}
	}

	private final def term(): Expr = {
		term2(factor())
	}

	private final def term2(e: Expr): Expr = {
		currentToken match {
			case BINOP(DIV) => {advance(); term2(BinaryExpr(e, DIV, factor(), Sinteger, tokens.line))}
			case BINOP(TIMES) => {advance(); term2(BinaryExpr(e, TIMES, factor(), Sinteger, tokens.line))}
			case _ => e
		}
	}

	private final def factor(): Expr = {
		currentToken match {
			case LEFT_PAREN => {advance(); val e = parseOr(); consume(RIGHT_PAREN); e}
			case IDENT(name) => {advance(); parseIdent(name, tokens.line)}
			case NUMBER(value) => {advance(); Num(value, Sinteger, tokens.line)}
			case TRUE => {advance(); BoolV(1, Sbool, tokens.line)}
			case FALSE => {advance(); BoolV(0, Sbool, tokens.line)}
			case MINUS => {advance(); UnaryExpr(UMINUS, factor(), Sinteger, tokens.line)}
			case BANG => {advance(); UnaryExpr(NOT, factor(), Sbool, tokens.line)}
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected '(', variable or number")
		}
	}

	private final def parseIdent(name: String, line: Int): Expr = {
		currentToken match {
			case LEFT_PAREN => FunCall(name, parseArgList(), Sinteger, line)
			case _ => Variable(name, Svoid, line)
		}
	}

	private final def parseArgList(): ListBuffer[Expr] = {
		val arg_list = ListBuffer[Expr]()
		while (tokens.hasNext && (currentToken != RIGHT_PAREN)) {
			currentToken match {
				case LEFT_PAREN => {advance(); arg_list += parseArg()}
				case COMMA => {advance(); arg_list += parseArg()}
				case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected '(' or ',' in list of arguments")
			}
		}
		consume(RIGHT_PAREN)
		arg_list
	}

	private final def parseArg(): Expr = {
		currentToken match {
			case RIGHT_PAREN => {
				Nil(Svoid, tokens.line)
			}
			case _ => parseOr()
		}
	}

	private final def consume(tok: Token): Unit = {
		if (tok == currentToken) advance() else throw ParserException("Syntax error on line " + tokens.line + ": expected '" + tok.toString +"'")
	}

	private final def advance(): Unit = {
		assert(tokens.hasNext)
		currentToken = tokens.next()
	}
}