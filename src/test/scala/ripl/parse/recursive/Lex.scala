package ripl.parse.recursive

import org.scalatest._

import ripl.ast.common._
import ripl.parse.recursive._

import ripl.reduce.CustomMatchers.matchAst

class TestLexer extends FreeSpec with Matchers {

  def test(name: String, input: String)(out: Token*): Unit = name in {
    Lex(input) should matchAst(out)
  }
  def test(input: String)(out: Token*): Unit = test(input, input)(out: _*)
  def testName(input: String): Unit = test(input, input)(Name(input))

  import Token._

  "formatting tokens" - {
    test("a single space", " ")()
    test("\n")(Newline)

    test("(")(LParen)
    test(")")(RParen)

    test("'")(Apostrophe)
    test("^")(Circumflex)
    test("~")(Tilda)
  }

  "atoms" - {
    "names" - {
      "simple" - {
        testName("John")
        testName("tim")
        testName("H2O")
      }
      "naming conventions" - {
        testName("camelCase")
        testName("StudlyCase")
        testName("snake_case")
        testName("SCREAMING_SNAKE_CASE")
        testName("_surrounded_with_under_scores_")
      }
      "names with keyword-like substrings" - {
        testName("andy")
        testName("ornate")
        testName("trueism")
        testName("falsely")
        testName("adrift")
        testName("earthen")
        testName("elsewhere")
      }
      "operators" - {
        testName("+")
        testName("-")
        testName("*")
        testName("/")
        testName("%")
        testName(":")
        testName("++")
        testName(">>=")
        testName("<>")
        testName("<$>")
        testName("<:>")
        testName("?!")
      }
      "with unicode" - {
        testName("λ")
        test("Maebe Fünke")(Name("Maebe"), Name("Fünke"))

        // Some test strings from http://www.columbia.edu/~fdc/utf8/
        testName("ᚠᛇᚻ᛫ᛒᛦᚦ")
        test("Τη γλώσσα")(Name("Τη"), Name("γλώσσα"))
      }
    }
    "boolean literals" - {
      test("true")(Name("true"))
      test("false")(Name("false"))
    }
    "integer literals" - {
      test("0")(VInt(0))
      test("4")(VInt(4))
      test("1536")(VInt(1536))
    }
    "floating point literals" - {
      test("0.0")(VFlt(0.0f))
      test("4.037")(VFlt(4.037f))
      test("0.019")(VFlt(0.019f))
    }
    "string literals" - {
      test("\"hello world!\"")(VStr("hello world!"))

    }
  }
  "comments" - {
    test("; comment")()
    test("a ; comment")(Name("a"))
    test("a ; comment b")(Name("a"))
    test("a ; comment \n b")(Name("a"), Name("b"))
  }

  "indentation" - {
    "delimited by whitespace" - {
      test(
        "  a"
      )(Indent, Name("a"), Dedent)
      test(
        """  a
          |  b""".stripMargin
      )(Indent, Name("a"), Newline, Name("b"), Dedent)

      test(
        """  a
          |    i""".stripMargin
      )(Indent, Name("a"), Newline, Indent, Name("i"), Dedent, Dedent)

      test(
        """  a
          |  b
          |    i
          |    j
          |    k""".stripMargin
      )(
        Indent,
        Name("a"),
        Newline,
        Name("b"),
        Newline,
        Indent,
        Name("i"),
        Newline,
        Name("j"),
        Newline,
        Name("k"),
        Dedent,
        Dedent
      )

      test(
        """  a
          |
          |  b
          |  c
          |    i
          |      x
          |  d
          |  e
          |
          |    j
          |  f""".stripMargin
      )(
        Indent,
        Name("a"),
        Newline,
        Newline,
        Name("b"),
        Newline,
        Name("c"),
        Newline,
        Indent,
        Name("i"),
        Newline,
        Indent,
        Name("x"),
        Newline,
        Dedent,
        Dedent,
        Name("d"),
        Newline,
        Name("e"),
        Newline,
        Newline,
        Indent,
        Name("j"),
        Newline,
        Dedent,
        Name("f"),
        Dedent
      )

      test(
        """  a
          |    i
          |      x
          |      y
          |    j
          |      z
          """.stripMargin
      )(
        Indent,
        Name("a"),
        Newline,
        Indent,
        Name("i"),
        Newline,
        Indent,
        Name("x"),
        Newline,
        Name("y"),
        Newline,
        Dedent,
        Name("j"),
        Newline,
        Indent,
        Name("z"),
        Newline,
        Dedent,
        Dedent,
        Dedent
      )
    }
  }
}
