package ripl.parse

import org.scalatest._

import ripl.ast.common._
import ripl.ast.untyped._
import ripl.parser._

import ripl.reduce.CustomMatchers.matchAst

class TestParser extends FreeSpec with Matchers {

  def test(input: String)(out: Node): Unit
    = Parse.exp(input) should matchAst(out)
  def testName(input: String): Unit
    = Parse.exp(input) should matchAst(Name(input))

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
        "- -7" in {
          test("- -7")(
            App(
              Name("-"),
              App(
                Name("-"),
                VInt(7))))
        }
        "-x" in {
          test("-x")(App(Name("-"), Name("x")))
        }
        "not true" in {
          test("not true")(App(Name("not"), VBln(true)))
        }
      }
      "binary operations" - {
        "4 + 5" in {
          test("4 + 5")(App(Name("+"), VInt(4), VInt(5)))
        }
        "4 - 5" in {
          test("4 - 5")(App(Name("-"), VInt(4), VInt(5)))
        }
        "are left-associative" - {
          "1 + 2 + 3 => (1 + 2) + 3" in {
            test("1 + 2 + 3")(
              App(
                Name("+"),
                App(
                  Name("+"),
                  VInt(1),
                  VInt(2)),
                VInt(3)))
          }
          "1 - 2 + 3 => (1 - 2) + 3" in {
            test("1 - 2 + 3")(
              App(
                Name("+"),
                App(
                  Name("-"),
                  VInt(1),
                  VInt(2)),
                VInt(3)))
          }
          "1 + 2 - 3 + 4 => ((1 + 2) - 3) + 4" in {
            test("1 + 2 - 3 + 4")(
              App(
                Name("+"),
                App(
                  Name("-"),
                  App(
                    Name("+"),
                    VInt(1),
                    VInt(2)),
                  VInt(3)),
                VInt(4)))
          }
        }
        "are subject to precedence" - {
          "a * x + b => (a * x) + b" in {
            test("a * x + b")(
              App(
                Name("+"),
                App(
                  Name("*"),
                  Name("a"),
                  Name("x")),
                Name("b")))
          }
          "a + x * b => a + (x * b)" in {
            test("a + x * b")(
              App(
                Name("+"),
                Name("a"),
                App(
                  Name("*"),
                  Name("x"),
                  Name("b"))))
          }
          "a / x + b => (a / x) + b" in {
            test("a / x + b")(
              App(
                Name("+"),
                App(
                  Name("/"),
                  Name("a"),
                  Name("x")),
                Name("b")))
          }
          "a % x + b => (a % x) + b" in {
            test("a % x + b")(
              App(
                Name("+"),
                App(
                  Name("%"),
                  Name("a"),
                  Name("x")),
                Name("b")))
          }
          "a == b and b == c" in {
            test("a == b and b == c")(
              App(
                Name("and"),
                App(
                  Name("=="),
                  Name("a"),
                  Name("b")),
                App(
                  Name("=="),
                  Name("b"),
                  Name("c"))))
          }
          "a and b or b and not c" in {
            test("a and b or b and not c")(
              App(
                Name("or"),
                App(
                  Name("and"),
                  Name("a"),
                  Name("b")),
                App(
                  Name("and"),
                  Name("b"),
                  App(
                    Name("not"),
                    Name("c")))))
          }
        }
      }
      "combined binary and unary operations" - {
        "-a + b" in {
          test("-a + b")(
            App(
              Name("+"),
              App(
                Name("-"),
                Name("a")),
              Name("b")))
        }
        "a + -b" in {
          test("a + -b")(
            App(
              Name("+"),
              Name("a"),
              App(
                Name("-"),
                Name("b"))))
        }
        "-(a + b)" in {
          test("-(a + b)")(
            App(
              Name("-"),
              App(
                Name("+"),
                Name("a"),
                Name("b"))))
        }
      }
      "function types" - {
        "Int -> Int" in {
          test("Int -> Int")(
            TFun(List(Name("Int")), Name("Int"))
          )
        }
        "(Int) -> Int" in {
          test("(Int) -> Int")(
            TFun(List(Name("Int")), Name("Int"))
          )
        }
        "(Int, Int) -> Bln" in {
          test("(Int, Int) -> Bln")(
            TFun(
              List(
                Name("Int"),
                Name("Int")),
              Name("Bln"))
          )
        }
        "Flt -> Int -> Bln" in {
          test("Flt -> Int -> Bln")(
            TFun(
              List(
                Name("Flt")),
              TFun(
                List(
                  Name("Int")),
                Name("Bln")
              )))
        }
        "(Int, Int) -> Int -> Int" in {
          test("(Int, Int) -> Int -> Int")(
            TFun(
              List(
                Name("Int"),
                Name("Int")),
              TFun(
                List(
                  Name("Int")),
                Name("Int")
              )))
        }
      }
      "if expressions" - {
        "if a then b else c" in {
          test("if a then b else c")(
            If(Name("a"), Name("b"), Name("c"))
          )
        }
        "if a then b else if c then d else e" in {
          test("if a then b else if c then d else e")(
            If(
              Name("a"),
              Name("b"),
              If(
                Name("c"),
                Name("d"),
                Name("e")))
          )
        }
        "if n <= 1 then 1 else n * fact(n - 1)" in {
          test("if n <= 1 then 1 else n * fact(n - 1)")(
            If(
              App(
                Name("<="),
                Name("n"),
                VInt(1)),
              VInt(1),
              App(
                Name("*"),
                Name("n"),
                App(
                  Name("fact"),
                  App(
                    Name("-"),
                    Name("n"),
                    VInt(1))))))

        }
      }
      "functions" - {
        "() => None" in {
          test("() => None")(
            Fun()(None)(
              Name("None")))
        }
        "(Int a) => a + 1" in {
          test("(Int a) => a + 1")(
            Fun(Param("a", Name("Int")))(None)(
              App(
                Name("+"),
                Name("a"),
                VInt(1))))
        }
        "(Int a) -> Int => a + 1" in {
          test("(Int a) -> Int => a + 1")(
            Fun(Param("a", Name("Int")))(Some(Name("Int")))(
              App(
                Name("+"),
                Name("a"),
                VInt(1))))
        }
        "(Int a, Int b) => a + b" in {
          test("(Int a, Int b) => a + b")(
            Fun(
              Param("a", Name("Int")),
              Param("b", Name("Int")))(
              None)(
              App(
                Name("+"),
                Name("a"),
                Name("b"))))
        }
        "(Int a, Int b) -> Int => a + b" in {
          test("(Int a, Int b) -> Int => a + b")(
            Fun(
              Param("a", Name("Int")),
              Param("b", Name("Int")))(
              Some(Name("Int")))(
              App(
                Name("+"),
                Name("a"),
                Name("b"))))
        }
        "(Int n) => if n <= 1 then 1 else n * fact(n - 1)" in {
          test("(Int a, Int b) => a + b")(
            Fun(
              Param("a", Name("Int")),
              Param("b", Name("Int")))(
              None)(
              App(
                Name("+"),
                Name("a"),
                Name("b"))))
        }
      }
    }

  }
}

