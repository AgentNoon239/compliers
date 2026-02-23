package cdim

import scala.io._
import scala.sys.process._
import java.io._
import cdim.error._

object Main {
	def main(args: Array[String]): Unit = {
		if (args.length < 1) {
			throw new Exception("There should be an input")
		} else if (args.length == 1) {
			val source = scala.io.Source.fromFile(args(0))
			val src = source.getLines()
			try run(src) catch {
				case c: ParserException => println(c.message)
				case c: NoSuchElementException => println("Looks like you're looking for elements that aren't there in an Iterator.")
			} finally source.close()
		}
	}

	private def run(src: Iterator[String]): Unit = {
		/* First creates the annotated abstract syntax tree with dummy annotations */
		val tree = new Parser(new Lexer(src)).parse()
		/* Keiko code file stored */
		val keikoFile = new PrintWriter(new File("a.k"))
		/* Writes to file after doing semantic analysis */
		keikoFile.write(new CodeGen(new Semantic(tree).check()).codeGen())
		keikoFile.close
		/* Executes the interpreter in the command line */
		val cmd = "./interpretk"
		/* Executes command */
		print(cmd.!!)
	}
	
}