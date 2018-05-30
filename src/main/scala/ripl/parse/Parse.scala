package ripl.parse

import org.antlr.v4.runtime._

import ripl.ast.untyped._
import ripl.parser.antlr._

case object Parse {

  private def getParser(input: String): RiplParser = {
    val charStream = new ANTLRInputStream(input)
    val lexer = new RiplLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    new RiplParser(tokens)
  }

  def exp(input: String): Node =
    ParseTreeToAst(getParser(input).exp2())
}

