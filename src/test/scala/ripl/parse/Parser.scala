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
        "John is a name" in {
          testName("John")
        }
        "tim is a name" in {
          testName("tim")
        }
        "H2O is a name" in {
          testName("H2O")
        }
        "camelCase is a name" in {
          testName("camelCase")
        }
        "StudlyCase is a name" in {
          testName("StudlyCase")
        }
        "snake_case is a name" in {
          testName("snake_case")
        }
        "SCREAMING_SNAKE_CASE is a name" in {
          testName("SCREAMING_SNAKE_CASE")
        }
        "_surrounded_in_under_scores_ is a name" in {
          testName("_surrounded_in_under_scores_")
        }
        "+ is a name" in {
          testName("+")
        }
        "- is a name" in {
          testName("-")
        }
        "* is a name" in {
          testName("*")
        }
        "/ is a name" in {
          testName("/")
        }
        "% is a name" in {
          testName("%")
        }
        "++ is a name" in {
          testName("++")
        }
        ">>= is a name" in {
          testName(">>=")
        }
        "--< is a name" in {
          testName("--<")
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

