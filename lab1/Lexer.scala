package cdim 
/*
This is the lexer (or scanner) class that chunks up the source file into strings where each
line in the source file is a string

The lexer is also an Iterator where the elements are the tokens from the source file
*/
import scala.collection.mutable.HashMap
import cdim.token._
import cdim.error._

class Lexer(val source: Iterator[String]) extends Iterator[Token] {

	/* Stores all the languages keywords */
	private val keywords = new HashMap[String, Token]()
	keywords += ("else" -> ELSE)
	keywords += ("for" -> FOR)
	keywords += ("if" -> IF)
	keywords += ("print" -> PRINT)
	keywords += ("while" -> WHILE)
	keywords += ("for" -> FOR)

	/* Regexes for leading characters in numbers and identifiers */
	private val digit = """[0-9]""".r
	private val id = """[a-zA-Z]""".r

	/* The lexer takes in an Iterator[String] where each element is line from the source file
       so we'll process each of these strings as a chunk one at a time and turn them into a token 
       stream */ 
	private var chunk = if (source.hasNext) source.next() else ""
	private var start = 0
	private var current = start
	
	/* Element number in source that is used in error reporting - is accessible to Parser */
	var line = 1

	/* Indicates if the EOF token has been produced yet */
	private var atEOF = false

	/* Lexer class extends Iterator[Token] so needs a next method */
	def next(): Token = {
		if (!atEOF) {
			scanToken()
		} else throw NoSuchElementException
	}

	/* Also need a hasNext method */
	def hasNext: Boolean = !atEOF

		/* Method to generate next token in the string or EOF if the end is reached */
	private def scanToken(): Token = {
		advance() match {
			/* single characters */
			case '(' => LEFT_PAREN
			case ')' => RIGHT_PAREN
			case '{' => LEFT_BRACE
			case '}' => RIGHT_BRACE
			case '-' => MINUS
			case '+' => BINOP(ADD)
			case '*' => BINOP(TIMES)
			case ';' => SEMICOLON
			/* double characters */
			case '!' => {if (isMatch('=')) RELOP(BANG_EQUAL) else BANG}
			case '&' => {if (isMatch('&')) BINOP(AND) else throw ParserException(s"Should be && on line $line at " + chunk.slice(start,current))}
			case '|' => {if (isMatch('|')) BINOP(OR) else throw ParserException(s"Should be || on line $line at " + chunk.slice(start,current))}
			case '=' => {if (isMatch('=')) RELOP(EQUAL_EQUAL) else EQUAL}
			case '<' => {if (isMatch('=')) RELOP(LESS_EQUAL) else RELOP(LESS)}
			case '>' => {if (isMatch('=')) RELOP(GREATER_EQUAL) else RELOP(GREATER)}
			case '/' => {if (isMatch('/')) nextChunk() else BINOP(DIV)}
			/* First checks if output of matchName(buffer) is in the keywords Hash table otherwise 
			   outputs the variable name token */
			case '0' => NUMBER(0)
			case this.id() => {
				val name = matchName(chunk(current - 1).toString)
				keywords.getOrElse(name, IDENT(name))
			}
 			case this.digit() => {
 				val number = matchDigit(chunk(current - 1).toString)
 				NUMBER(number.toInt)
 			}

 			case ' ' => scanToken()
 			case '\r' => scanToken()
 			case '\t' => scanToken()
 			/* If we have reached the end of the chunk we assign a new line to chunk or 
 			   end of file token */
 			case '\u0000' => nextChunk()
  			case _ => throw ParserException(s"unexpected character on line $line at " + chunk.slice(start,current))
 		}
	}

	// Helper methods

	/* Useful for encountering comments or the null character */
	private def nextChunk(): Token = {
		if (source.hasNext) {
 			chunk = source.next()
 			line = line + 1
 			start = 0
		    current = start
 			scanToken()
 		} else {
 			atEOF = true
 			EOF
 		}
	}

	/* Determines if current index has exceeded source's non-null characters */
	private def isAtEnd: Boolean = {
		assert(current >= 0)
		current >= chunk.length
	}

	/* Retrieve's current character and moves onto next */
	private def advance(): Char = {
		assert(current <= chunk.length)
		val c = if (isAtEnd) '\u0000' else chunk.charAt(current)
		current = current + 1
		c
	}

	/* Checks if current character is a match with input and advance if so */
	private def isMatch(char: Char): Boolean = {
		if (isAtEnd) return false 
		if (chunk.charAt(current) != char) return false
		current = current + 1
		true
	}

	/* Check for longest match for names and integers - the substrings start at current - 1 as
	   leading character has been read already */
	private val digit_tail = """[0-9]""".r
	private def matchDigit(str: String): String = {
		assert(current > 0)
		advance() match {
			case this.digit_tail() => matchDigit(str + chunk(current - 1))
			case _ => {current = current - 1; str}
		}
	}

	private val id_tail = """[a-zA-Z0-9_]""".r
	private def matchName(str: String): String = {
		assert(current > 0)
		advance() match {
			case this.id_tail() => matchName(str + chunk(current - 1))
			case _ => {current = current - 1; str}
		}
	}
}
