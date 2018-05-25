package ripl.parse

import org.scalatest._

import ripl.ast.common._
import ripl.ast.untyped._
import ripl.parser._

import ripl.reduce.CustomMatchers.matchAst

class TestParser extends FreeSpec with Matchers {

  def test(input: String)(out: Node): Unit = Parser(input) should matchAst(out)
  def testName(input: String): Unit = Parser(input) should matchAst(Name(input))

  "expressions" - {
    "exp0" - {
      "names" - {
        "simple names" - {
          "John" in {
            testName("John")
          }
          "tim" in {
            testName("tim")
          }
          "H2O" in {
            testName("H2O")
          }
        }
        "naming conventions" - {
          "camelCase" in {
            testName("camelCase")
          }
          "StudlyCase" in {
            testName("StudlyCase")
          }
          "snake_case" in {
            testName("snake_case")
          }
          "SCREAMING_SNAKE_CASE" in {
            testName("SCREAMING_SNAKE_CASE")
          }
          "_surrounded_with_under_scores_" in {
            testName("_surrounded_with_under_scores_")
          }
        }
        "names with keyword substrings" - {
          "truest" in {
            testName("truest")
          }
          "falsely" in {
            testName("falsely")
          }
          "iffith" in {
            testName("iffith")
          }
          "thennis" in {
            testName("thennis")
          }
          "elsely" in {
            testName("elsely")
          }
        }
        "operators" - {
          "+" in {
            testName("+")
          }
          "-" in {
            testName("-")
          }
          "*" in {
            testName("*")
          }
          "/" in {
            testName("/")
          }
          "%" in {
            testName("%")
          }
          ":" in {
            testName(":")
          }
          "++" in {
            testName("++")
          }
          ">>=" in {
            testName(">>=")
          }
          "<>" in {
            testName("<>")
          }
          "<$>" in {
            testName("<$>")
          }
          "<:>" in {
            testName("<:>")
          }
          "?!" in {
            testName("?!")
          }
        }
      }
      "boolean literals" - {
        "true" in {
          test("true")(VBln(true))
        }
        "false" in {
          test("false")(VBln(false))
        }
      }
      "integer literals" - {
        "0" in {
          test("0")(VInt(0))
        }
        "4" in {
          test("4")(VInt(4))
        }
      }
      "floating point literals" - {
        "0.0" in {
          test("0.0")(VFlt(0.0f))

        }
        "4.037" in {
          test("4.037")(VFlt(4.037f))
        }
        "0.019" in {
          test("0.019")(VFlt(0.019f))
        }
      }
      "bracketed expressions" - {
        "a * (x + b)" in {
          test("a * (x + b)")(
            App(
              Name("*"),
              Name("a"),
              App(
                Name("+"),
                Name("x"),
                Name("b"))))
        }
      }
    }
    "exp1" - {
      "unary operations" - {
        "-7" in {
          test("-7")(App(Name("-"), VInt(7)))
        }
        "-x" in {
          test("-x")(App(Name("-"), Name("x")))
        }
      }
      "binary operations" - {
        "4 + 5" in {
          test("4 + 5")(App(Name("+"), VInt(4), VInt(5)))
        }
        "1 + 2 + 3 parsed as (1 + 2) + 3" in {
          test("1 + 2 + 3")(
            App(
              Name("+"),
              App(
                Name("+"),
                VInt(1),
                VInt(2)),
              VInt(3)))
        }
        "a * x + b parsed as (a * x) + b" in {
          test("a * x + b")(
            App(
              Name("+"),
              App(
                Name("*"),
                Name("a"),
                Name("x")),
              Name("b")))
        }
        "a + x * b parsed as a + (x * b)" in {
          test("a + x * b")(
            App(
              Name("+"),
              Name("a"),
              App(
                Name("*"),
                Name("x"),
                Name("b"))))
        }
      }
    }

  }
}

