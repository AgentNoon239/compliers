package cdim
/*
This is the parser takes the lexer as a parameter and has only one public method: parse()

It is a recursive descent parser that will call the lexer iterator methods when it needs new tokens 
*/
import scala.collection.mutable.ListBuffer 

import cdim.ast._
import cdim.token._
import cdim.error._

class Parser (val tokens: Lexer) {

	/* The main public method that produces a root node for the AST of trait Prog, which has a Stmt list as its children */
	def parse(): Prog = {
		val stmt_list = ListBuffer[Stmt]()
		while(tokens.hasNext) {
			stmt_list += parseStmt()
		}
		Program(stmt_list)
	}

	/* Parser works by inspecting the next token - this is stored in currentToken */
	private var currentToken = if (tokens.hasNext) tokens.next() else throw NoSuchElementException

	/* Parse corresponding to Stmt grammar */
	private def parseStmt(): Stmt = {
		currentToken match {
			case LEFT_BRACE => {advance(); val stmts = block(); consume(RIGHT_BRACE); stmts}
			case PRINT => {advance(); parsePrint()}
			case IDENT(name) => {parseIdent(name)}
			case IF => {advance(); parseIf()}
			case WHILE => {advance(); parseWhile()}
			case FOR => {advance(); parseFor()}
			case SEMICOLON => {advance(); EmptyStmt}
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected a statement or ';'")
		}
	}

	/* This parses the blocks of Stmts, e.g. {a = 2; b = 2;} into a Stmt list */
	/* Does some simplification if the list contains fewer than 2 elements */
	private def block(): Stmt = {
		val stmt_list = ListBuffer[Stmt]()
		while(tokens.hasNext && (currentToken != RIGHT_BRACE)) {
			stmt_list += parseStmt()
		}
		if (stmt_list.isEmpty) return EmptyStmt
		else if (stmt_list.length == 1) return stmt_list(0)
		else SeqStmt(stmt_list)
	}

	private def parseIdent(name: String): AssignStmt = {
		advance()
		consume(EQUAL)
		val e = expr()
		consume(SEMICOLON)
		AssignStmt(name, e)
	}

	/* Parses print statements */
	private def parsePrint(): Stmt = {
		consume(LEFT_PAREN)
		val e = expr()
		consume(RIGHT_PAREN)
		consume(SEMICOLON)
		PrintStmt(e)
	}

	/* Parses if statements and else statements greedily */
	private def parseIf(): Stmt = {
		consume(LEFT_PAREN)
		val cond = expr()
		consume(RIGHT_PAREN)
		val thenst = parseStmt()
		currentToken match {
			case ELSE => {advance(); IfStmt(cond,thenst,parseStmt())}
			case _ => IfStmt(cond,thenst,EmptyStmt)
		}
	}

	/* Parses while statements */
	private def parseWhile(): Stmt = {
		consume(LEFT_PAREN)
		val cond = expr()
		consume(RIGHT_PAREN)
		WhileStmt(cond, parseStmt())
	}

	private def parseFor(): Stmt = {
		consume(LEFT_PAREN)
		val iterator = currentToken match {
			case IDENT(name) => parseIdent(name)
			case _ => throw ParserException("Expected ident")
		}
		val exprf = expr()
		consume(SEMICOLON)
		val exprinc = expr()
		consume(RIGHT_PAREN)
		ForStmt(iterator.name, iterator.expr, exprf, exprinc, parseStmt())
	}

	/* The grammar in the specification in Notes.md is left-recursive so not appropriate */

	/* Expr -> CalcExpr Expr2 */
	/* Expr2 -> RELOP(op) CalcExpr Expr2 | epsilon */

	/* To have left-associativity we pass the result from parsing CalcExpr into the function expr2() */
	/* That is, an already constructed tree is passed into expr2 and if a RELOP is matched, recursively build on to
	   the tree (first production) or just return the input as output for the other production */

	private def expr(): Expr = {
		expr2(logicExpr())
	}

	private def expr2(exprcond: Expr): Expr = {
		currentToken match {
			case QUESTION_MARK => {
				advance(); 
				val exprt = logicExpr();
				consume(COLON);
				val exprf = logicExpr();
				TernaryExpr(exprcond,exprt,exprf)
			}
			case _ => exprcond
		}
	}

	private def logicExpr(): Expr = {
		logicExpr2(calcExpr())
	}

	private def logicExpr2(e: Expr): Expr = {
		currentToken match {
			case RELOP(op) => {advance(); expr2(BinaryExpr(e, op, calcExpr()))}
			case _ => e
		}
	}

	/* CalcExpr -> Term CalcExpr2 */
	/* CalcExpr2 -> BINOP(op) Term CalcExpr2 | epsilon */
	/* for op having the order of precedence of addition */

	private def calcExpr(): Expr = {
		calcExpr2(term())
	}


	private def calcExpr2(e: Expr): Expr = {
		currentToken match {
			case MINUS => {advance(); calcExpr2(BinaryExpr(e, SUBTRACT, term()))}
			case BINOP(ADD) => {advance(); calcExpr2(BinaryExpr(e, ADD, term()))}
			case BINOP(OR) => {advance(); calcExpr2(BinaryExpr(e, OR, term()))}
			case _ => e
		}
	}

	private def choices(): (Expr,Expr) = {
		choices2(expr())
	}

	private def choices2(e: Expr): (Expr,Expr) = {
		currentToken match {
			case COLON => {
				advance(); 
				(e,expr())
			}
			case _ => throw ParserException("Syntax error expected : in ternary statement")
		}
	}

	/* Term -> Factor Term2 */
	/* Term2 -> BINOP(op) Factor Term2 | epsilon */
	/* for op having the order of precedence of multiplication */

	private def term(): Expr = {
		term2(factor())
	}

	private def term2(e: Expr): Expr = {
		currentToken match {
			case BINOP(DIV) => {advance(); term2(BinaryExpr(e, DIV, factor()))}
			case BINOP(TIMES) => {advance(); term2(BinaryExpr(e, TIMES, factor()))}
			case BINOP(AND) => {advance(); term2(BinaryExpr(e, AND, factor()))}
			case _ => e
		}
	}

	/* Factor -> '(' Expr ')'
	           | 'IDENT' 
	           | 'NUMBER'
	           | '-' Factor
	           | "!" Factor */

	private def factor(): Expr = {
		currentToken match {
			case LEFT_PAREN => {advance(); val e = expr(); consume(RIGHT_PAREN); e}
			case IDENT(name) => {advance(); Variable(name)}
			case NUMBER(value) => {advance(); Num(value)}
			case MINUS => {advance(); UnaryExpr(UMINUS, factor())}
			case BANG => {advance(); UnaryExpr(NOT, factor())}
			case _ => throw ParserException("Syntax error on line " + tokens.line + ": expected '(', variable or number found " + currentToken.toString)
		}
	}

	/* Helper function that consumes a token if it matches */

	private def consume(tok: Token): Unit = {
		if (tok == currentToken) advance() else throw ParserException("Syntax error on line " + tokens.line + ": expected '" + tok.toString +"' found " + currentToken.toString)
	}

	/* Advances through the token stream */

	private def advance(): Unit = {
		assert(tokens.hasNext)
		currentToken = tokens.next()
	}
}


