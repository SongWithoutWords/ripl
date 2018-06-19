package ripl.parse.recursive

import org.scalatest._

import ripl.parse.recursive._

import ripl.reduce.CustomMatchers.matchAst

class TestLexer extends FreeSpec with Matchers {

  def test(name: String, input: String)(out: Token*): Unit = name in {
    Lex(input) shouldBe out
  }
  def test(input: String)(out: Token*): Unit =
    test(input, input)(out: _*)

  import Token._

  "single tokens" - {
    test("a single space", " ")()
    test("\n")(Newline)

    test("(")(LParen)
    test(")")(RParen)

    test("'")(Apostrophe)
    test("^")(Circumflex)
    test("~")(Tilda)

  }

}
