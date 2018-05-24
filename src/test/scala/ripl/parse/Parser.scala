package ripl.parser

import org.scalatest._

import ripl.ast.untyped._
import ripl.parser._

import ripl.reduce.CustomMatchers.matchAst

class TestParser extends FreeSpec with Matchers {

  def test(input: String)(out: Node): Unit = Parser(input) should matchAst(out)

  "exp0" - {
    "names" - {
      "John is a name" in {
        test("John")(Name("John"))
      }
    }
    "integer literals" - {

    }
    "floating point literals" - {

    }
    "bracketed expressions" - {

    }
  }

}

