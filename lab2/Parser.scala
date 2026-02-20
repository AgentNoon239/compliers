package cdim

import scala.collection.mutable.ListBuffer 

import cdim.ast._
import cdim.token._
import cdim.error._

class Parser (val tokens: Lexer) {

	def parse(): Prog = {
		Program(parseDecls(), parseStmts())
	}

	private final var currentToken = if (tokens.hasNext) tokens.next() else throw NoSuchElementException

	/* Parses ;-separated declarations, e.g. int a,b; bool c;
		where each declaration can be a list of names of same type */
	/* Decls -> Decl SEMICOLON Decls' ; Decls' -> Decls | epsilon */
	/* Note that each individual decl here is also a list of declarations 
		since we can have comma-separated lists */
	private final def parseDecls(): ListBuffer[Decl] = {
		var decl_list = ListBuffer[Decl]()
		var decl = parseDecl()
		while (!decl.isEmpty) {
			consume(SEMICOLON)
			decl_list = decl_list ++ decl
			decl = parseDecl()
		}
		decl_list
	}

	/* Decl -> TYPE Names */
	private final def parseDecl(): ListBuffer[Decl] = {
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

	/* Names -> Name Name' ; Name' -> COMMA Names | epsilon  */
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

	/* Name -> IDENT Name' ; Name' -> Dims */
	private final def parseName(stype: Stype): Decl = {
		currentToken match {
			case IDENT(name) => {
				advance()
				Declr(name, parseBounds(stype), tokens.line)
			}
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected variable name")
		}
	}

	/* Dims -> (LEFT_SQ NUMBER RIGHT_SQ)* */
	private final def parseBounds(stype: Stype): Stype = {
		val dim = parseBound()
		if (dim == -1) {
			stype
		} else {
			Sarray(dim, parseBounds(stype))
		}
	}

	private final def parseBound(): Int = {
		currentToken match {
			case LEFT_SQ => {
				advance()
				val bound = parseDim()
				consume(RIGHT_SQ)
				bound
			}
			case COMMA => -1
			case SEMICOLON => -1
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected ',', ';' or '['")
		}
	}

	/* This parses the numbers inside LEFT_SQ and RIGHT_SQ */
	private final def parseDim(): Int = {
		currentToken match {
			case NUMBER(value) => if (value > 0) {advance(); value} else throw ParserException("Error on line " + tokens.line + ": array dimension should be number greater than zero")
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected array dimension as number")
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
			case LEFT_BRACE => {advance(); val stmts = block(); consume(RIGHT_BRACE); stmts}
			case PRINT => {advance(); parsePrint()}
			case IDENT(name) => {
				advance()
				val expr_left = parseVariable(name, tokens.line)
				consume(EQUAL)
				val expr_right = parseOr()
				consume(SEMICOLON)
				AssignStmt(expr_left, expr_right)
			}
			case IF => {advance(); parseIf()}
			case WHILE => {advance(); parseWhile()}
			case SEMICOLON => {advance(); EmptyStmt}
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected a statement or ';'")
		}
	}

	private final def block(): Stmt = {
		val decls_list = parseDecls()
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

	private def parseOr(): Expr = {
		parseOr2(parseAnd())
	}

	private def parseOr2(e: Expr): Expr = {
		currentToken match {
			case BINOP(OR) => {advance(); parseOr2(BinaryExpr(e, OR, parseAnd(), Sbool, tokens.line))}
			case _ => e
		}
	}

	private def parseAnd(): Expr = {
		parseAnd2(expr())
	}

	private def parseAnd2(e: Expr): Expr = {
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
			case IDENT(name) => {advance(); parseVariable(name, tokens.line)}
			case NUMBER(value) => {advance(); Num(value, Sinteger, tokens.line)}
			case TRUE => {advance(); BoolV(1, Sbool, tokens.line)}
			case FALSE => {advance(); BoolV(0, Sbool, tokens.line)}
			case MINUS => {advance(); UnaryExpr(UMINUS, factor(), Sinteger, tokens.line)}
			case BANG => {advance(); UnaryExpr(NOT, factor(), Sbool, tokens.line)}
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected '(', variable or number")
		}
	}

	private final def parseVariable(name: String, line: Int): Expr = {
		val expr_list = parseExprList()
		if (expr_list.isEmpty) {
			Variable(name, Svoid, line)
		} else {
			ArrayElement(name, expr_list, Svoid, line)
		}
	}

	private final def parseExprList(): ListBuffer[Expr] = {
		val expr_list = ListBuffer[Expr]()
		while (currentToken == LEFT_SQ) {
			expr_list += parseExpr()
		}
		expr_list
	}

	private final def parseExpr(): Expr = {
		consume(LEFT_SQ)
		val e = calcExpr()
		consume(RIGHT_SQ)
		e
	}

	private final def consume(tok: Token): Unit = {
		if (tok == currentToken) advance() else throw ParserException("Syntax error on line " + tokens.line + ": expected '" + tok.toString +"'")
	}

	private final def advance(): Unit = {
		assert(tokens.hasNext)
		currentToken = tokens.next()
	}
}