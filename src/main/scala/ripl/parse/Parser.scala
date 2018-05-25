package ripl.parse

import org.antlr.v4.runtime._

import ripl.ast.untyped._
import ripl.parser.antlr._

case object Parse {

  private def getRiplParser(input: String): riplParser = {
    val charStream = new ANTLRInputStream(input)
    val lexer = new riplLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    new riplParser(tokens)
  }

  def exp(input: String): Node =
    ParseTreeToAst(getRiplParser(input).exp2())
}

