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
  def testSymbol(input: String): Unit =
    test(input, input)(Token.Symbol(input))

  import Token._

  "single tokens" - {
    test("a single space", " ")()
    test("\n")(Newline)

    test("(")(LParen)
    test(")")(RParen)

    test("'")(Apostrophe)
    test("^")(Circumflex)
    test("~")(Tilda)

    "symbols" - {
      "simple" - {
        testSymbol("John")
        testSymbol("tim")
        testSymbol("H2O")
      }
      "naming conventions" - {
        testSymbol("camelCase")
        testSymbol("StudlyCase")
        testSymbol("snake_case")
        testSymbol("SCREAMING_SNAKE_CASE")
        testSymbol("_surrounded_with_under_scores_")
      }
      "names with keyword-like substrings" - {
        testSymbol("andy")
        testSymbol("ornate")
        testSymbol("trueism")
        testSymbol("falsely")
        testSymbol("adrift")
        testSymbol("earthen")
        testSymbol("elsewhere")
      }
      "operators" - {
        testSymbol("+")
        testSymbol("-")
        testSymbol("*")
        testSymbol("/")
        testSymbol("%")
        testSymbol(":")
        testSymbol("++")
        testSymbol(">>=")
        testSymbol("<>")
        testSymbol("<$>")
        testSymbol("<:>")
        testSymbol("?!")
      }
      "symbols with unicode" - {
        testSymbol("λ")
        test("Maebe Fünke")(Symbol("Maebe"), Symbol("Fünke"))

        // Some test strings from http://www.columbia.edu/~fdc/utf8/
        testSymbol("ᚠᛇᚻ᛫ᛒᛦᚦ")
        test("Τη γλώσσα")(Symbol("Τη"), Symbol("γλώσσα"))
      }
    }
  }

  "groups of tokens" - {}
}
