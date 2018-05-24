package ripl.parser

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
        }
      }
      "integer literals" - {
        "0 is an integer" in {
          test("0")(VInt(0))
        }
        "4 is an integer" in {
          test("4")(VInt(4))
        }


      }
      "floating point literals" - {

      }
      "bracketed expressions" - {

      }
    }
  }
}

