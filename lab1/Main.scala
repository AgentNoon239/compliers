package cdim
/* Main object that takes the input file, turns it into an Iterator[String] and parses it */
import scala.io._
import cdim.error._

object Main {
	def main(args: Array[String]): Unit = {
		if (args.length < 1) {
			throw new Exception("There should be an input file")
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
		val parser = new Parser(new Lexer(src))
		val tree = parser.parse()
		val interpreter = new Interpreter(tree)
		interpreter.execute()
	}
}