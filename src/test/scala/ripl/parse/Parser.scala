package ripl.parse

import org.scalatest._

import ripl.ast.common._
import ripl.ast.untyped._
import ripl.parser._
import ripl.util.MultiMap

import ripl.reduce.CustomMatchers.matchAst

class TestParser extends FreeSpec with Matchers {

  // `name in { block }` is ScalaTest's free-spec syntax
  def test(input: String)(out: Node): Unit = input in {
    Parse.exp(input) should matchAst(out)
  }

  def testAst(input: String)(out: (String, Node)*): Unit = input in {
    Parse.units(input) should matchAst(out.toList)
  }

  def testName(input: String): Unit = input in {
    Parse.exp(input) should matchAst(Name(input))
  }

  "expressions" - {
    "exp0" - {
      "names" - {
        "simple names" - {
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
        "names with keyword substrings" - {
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
      }
      "boolean literals" - {
        test("true")(VBln(true))
        test("false")(VBln(false))
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

        test("if value then \"true\" else \"false\"")(
          If(Name("value"), VStr("true"), VStr("false"))
        )
      }
      "bracketed expressions" - {
        test("a * (x + b)") {
          App(Name("*"), Name("a"), App(Name("+"), Name("x"), Name("b")))
        }
      }
    }
    "exp1" - {
      "unary operations" - {
        test("-7")(App(Name("-"), VInt(7)))
        test("- -7")(App(Name("-"), App(Name("-"), VInt(7))))
        test("-x")(App(Name("-"), Name("x")))
        test("not true")(App(Name("not"), VBln(true)))
      }
      "binary operations" - {
        test("4 + 5")(App(Name("+"), VInt(4), VInt(5)))
        test("4 - 5")(App(Name("-"), VInt(4), VInt(5)))
        "are left-associative" - {
          test("1 + 2 + 3")(
            App(Name("+"), App(Name("+"), VInt(1), VInt(2)), VInt(3))
          )
          test("1 - 2 + 3")(
            App(Name("+"), App(Name("-"), VInt(1), VInt(2)), VInt(3))
          )
          test("1 + 2 - 3 + 4")(
            App(
              Name("+"),
              App(Name("-"), App(Name("+"), VInt(1), VInt(2)), VInt(3)),
              VInt(4)
            )
          )
        }
        "are subject to precedence" - {
          test("a * x + b")(
            App(Name("+"), App(Name("*"), Name("a"), Name("x")), Name("b"))
          )
          test("a + x * b")(
            App(Name("+"), Name("a"), App(Name("*"), Name("x"), Name("b")))
          )
          test("a / x + b")(
            App(Name("+"), App(Name("/"), Name("a"), Name("x")), Name("b"))
          )
          test("a % x + b")(
            App(Name("+"), App(Name("%"), Name("a"), Name("x")), Name("b"))
          )
          test("a == b and b == c")(
            App(
              Name("and"),
              App(Name("=="), Name("a"), Name("b")),
              App(Name("=="), Name("b"), Name("c"))
            )
          )
          test("a and b or b and not c")(
            App(
              Name("or"),
              App(Name("and"), Name("a"), Name("b")),
              App(Name("and"), Name("b"), App(Name("not"), Name("c")))
            )
          )
          test("character.isHappy and character.isHealthy")(
            App(
              Name("and"),
              Select(Name("character"), "isHappy"),
              Select(Name("character"), "isHealthy")
            )
          )
        }
      }
      "combined binary and unary operations" - {
        test("-a + b")(App(Name("+"), App(Name("-"), Name("a")), Name("b")))
        test("a + -b")(App(Name("+"), Name("a"), App(Name("-"), Name("b"))))
        test("-(a + b)")(App(Name("-"), App(Name("+"), Name("a"), Name("b"))))
        test("character.isHungry and not character.isBusy") {
          App(
            Name("and"),
            Select(Name("character"), "isHungry"),
            App(Name("not"), Select(Name("character"), "isBusy"))
          )
        }
      }
      "function types" - {
        test("Int -> Int") {
          TFun(List(Name("Int")), Name("Int"))
        }
        test("(Int) -> Int") {
          TFun(List(Name("Int")), Name("Int"))
        }
        test("(Int, Int) -> Bln")(
          TFun(List(Name("Int"), Name("Int")), Name("Bln"))
        )
        test("Flt -> Int -> Bln")(
          TFun(
            List(Name("Flt")),
            TFun(
              List(Name("Int")),
              Name("Bln")
            )
          )
        )
        test("(Int, Int) -> Int -> Int")(
          TFun(
            List(Name("Int"), Name("Int")),
            TFun(
              List(Name("Int")),
              Name("Int")
            )
          )
        )
      }
      "if expressions" - {
        test("if a then b else c")(
          If(Name("a"), Name("b"), Name("c"))
        )
        test("if a then b else if c then d else e")(
          If(Name("a"), Name("b"), If(Name("c"), Name("d"), Name("e")))
        )
        test("if n <= 1 then 1 else n * fact(n - 1)")(
          If(
            App(Name("<="), Name("n"), VInt(1)),
            VInt(1),
            App(
              Name("*"),
              Name("n"),
              App(Name("fact"), App(Name("-"), Name("n"), VInt(1)))
            )
          )
        )
      }
      "functions" - {
        test("() => None")(Fun()(None)(Name("None")))
        test("(Int a) => a + 1")(
          Fun(Param("a", Name("Int")))(None)(App(Name("+"), Name("a"), VInt(1)))
        )
        test("(Int a) -> Int => a + 1")(
          Fun(Param("a", Name("Int")))(Some(Name("Int")))(
            App(Name("+"), Name("a"), VInt(1))
          )
        )
        test("(Int a, Int b) => a + b")(
          Fun(Param("a", Name("Int")), Param("b", Name("Int")))(None)(
            App(Name("+"), Name("a"), Name("b"))
          )
        )
        test("(Int a, Int b) -> Int => a + b")(
          Fun(Param("a", Name("Int")), Param("b", Name("Int")))(
            Some(Name("Int"))
          )(App(Name("+"), Name("a"), Name("b")))
        )

        test("(Int n) => if n <= 1 then 1 else n * fact(n - 1)")(
          Fun(Param("n", Name("Int")))(None)(
            If(
              App(Name("<="), Name("n"), VInt(1)),
              VInt(1),
              App(
                Name("*"),
                Name("n"),
                App(Name("fact"), App(Name("-"), Name("n"), VInt(1)))
              )
            )
          )
        )
      }
      "blocks" - {
        "delimited by punctuation" - {
          test("{}") {
            Block()
          }
          test("{{}}") {
            Block(Block())
          }
          test("{a}") {
            Block(Name("a"))
          }
          test("{ a }") {
            Block(Name("a"))
          }
          test("{ a; }") {
            Block(Name("a"))
          }
          test("{ a; b }") {
            Block(Name("a"), Name("b"))
          }
          test("{ a; b; { i; j; k }; x; y }") {
            Block(
              Name("a"),
              Name("b"),
              Block(Name("i"), Name("j"), Name("k")),
              Name("x"),
              Name("y"),
            )
          }
        }
        "delimited by whitespace" - {
          test(
            "  a"
          )(Block(Name("a")))
          test(
            """  a
              |  b""".stripMargin
          )(Block(Name("a"), Name("b")))

          test(
            """  a
              |    i""".stripMargin
          )(Block(Name("a"), Block(Name("i"))))

          test(
            """  a
              |  b
              |    i
              |    j
              |    k""".stripMargin
          )(Block(Name("a"), Name("b"), Block(Name("i"), Name("j"), Name("k"))))

          test(
            """  a
              |
              |  b
              |  c
              |    i
              |  d
              |  e
              |
              |    j
              |  f""".stripMargin
          )(
            Block(
              Name("a"),
              Name("b"),
              Name("c"),
              Block(Name("i")),
              Name("d"),
              Name("e"),
              Block(Name("j")),
              Name("f")
            )
          )

          test(
            """  a
              |    i
              |      x
              |      y
              |    j
              |      z
              |
              |  b
              |  c""".stripMargin
          )(
            Block(
              Name("a"),
              Block(
                Name("i"),
                Block(Name("x"), Name("y")),
                Name("j"),
                Block(Name("z"))
              ),
              Name("b"),
              Name("c")
            )
          )
        }
      }
      "user-defined types" - {
        "data syntax" - {
          test("data Empty")(Struct("Empty"))
          test("data Empty {}")(Struct("Empty"))
          test("data Vector { f32 x; f32 y }")(
            Struct("Vector", "x" -> Name("f32"), "y" -> Name("f32"))
          )
          test(
            """data Vector
              |  f32 x; f32 y""".stripMargin
          )(Struct("Vector", "x" -> Name("f32"), "y" -> Name("f32")))
          test(
            """data Vector
              |  f32 x
              |  f32 y""".stripMargin
          )(Struct("Vector", "x" -> Name("f32"), "y" -> Name("f32")))
          test(
            """data Colour
              |  i8 red
              |  i8 green
              |  i8 blue""".stripMargin
          )(
            Struct(
              "Colour",
              "red"   -> Name("i8"),
              "green" -> Name("i8"),
              "blue"  -> Name("i8")
            )
          )
        }
        "union syntax" - {
          test("union Empty")(Union("Empty", Nil))
          test("union Empty {}")(Union("Empty", Nil))
          test("union A_or_B { A; B }")(
            Union("A_or_B", List(Name("A"), Name("B")))
          )
        }
        "combined union and data syntax" - {
          test(
            "union MaybeI32 { data Some { i32 value }; data None }"
          )(
            Union(
              "MaybeI32",
              List(Struct("Some", "value" -> Name("i32")), Struct("None"))
            )
          )
          test(
            """union MaybeI32
              |  data Some { i32 value }
              |  data None""".stripMargin
          )(
            Union(
              "MaybeI32",
              List(Struct("Some", "value" -> Name("i32")), Struct("None"))
            )
          )

          val expUserType =
            Union(
              "Exp",
              List(
                Name("f32"),
                Struct("Add", "e1" -> Name("Exp"), "e2" -> Name("Exp")),
                Struct("Sub", "e1" -> Name("Exp"), "e2" -> Name("Exp")),
                Struct("Mul", "e1" -> Name("Exp"), "e2" -> Name("Exp")),
                Struct("Div", "e1" -> Name("Exp"), "e2" -> Name("Exp"))
              )
            )
          test(
            """union Exp
              |  f32
              |  data Add { Exp e1; Exp e2 }
              |  data Sub { Exp e1; Exp e2 }
              |  data Mul { Exp e1; Exp e2 }
              |  data Div { Exp e1; Exp e2 }""".stripMargin
          )(expUserType)

          test(
            """union Exp
              |  f32
              |  data Add { Exp e1; Exp e2; }
              |  data Sub { Exp e1; Exp e2 }
              |  data Mul
              |    Exp e1; Exp e2
              |  data Div
              |    Exp e1
              |    Exp e2""".stripMargin
          )(expUserType)

          test(
            """union Exp
              |  f32
              |  data Add
              |     Exp e1
              |     Exp e2
              |   data Sub
              |     Exp e1
              |     Exp e2
              |   data Mul
              |     Exp e1
              |     Exp e2
              |   data Div
              |     Exp e1
              |     Exp e2""".stripMargin
          )(expUserType)
        }
      }
      "variable definition" - {
        test("$ x = 0")(
          Define(Name("x"), Cons(Name("$"), VInt(0)))
        )
        test("~$ x = 0")(
          Define(Name("x"), Cons(App(Name("~"), Name("$")), VInt(0)))
        )
        test("$ pi = 3.14159265")(
          Define(Name("pi"), Cons(Name("$"), VFlt(3.14159265f)))
        )
        test("$ myClothing = if isRaining then rainGear else normalClothes")(
          Define(
            Name("myClothing"),
            Cons(
              Name("$"),
              If(Name("isRaining"), Name("rainGear"), Name("normalClothes"))
            )
          )
        )
      }
      "assignment" - {
        test("x = 0")(Assign(Name("x"), VInt(0)))
        test("x = x + 1")(Assign(Name("x"), App(Name("+"), Name("x"), VInt(1))))
      }
    }
  }
  "units" - {
    "variables" - {
      testAst("$ x = 0")(
        "x" -> Cons(Name("$"), VInt(0))
      )
    }
    "functions" - {

      testAst("inc(^~Int x) => x = x + 1")(
        "inc" ->
          Fun(
            List(Param("x", App(Name("^"), App(Name("~"), Name("Int"))))),
            None,
            Assign(Name("x"), App(Name("+"), Name("x"), VInt(1)))
          )
      )

    }
    "user-defined types" - {
      testAst("data Vector { f32 x; f32 y }")(
        "Vector" ->
          Struct("Vector", "x" -> Name("f32"), "y" -> Name("f32"))
      )
    }
    "namespaces" - {
      testAst("namespace empty")("empty" -> Namespace())

      testAst(
        """namespace ripl
          |  namespace math
          |    $ pi = 3.14159265""".stripMargin
      )(
        "ripl" -> Namespace(
          "math" -> Namespace("pi" -> Cons(Name("$"), VFlt(3.14159265f)))
        )
      )
      testAst(
        """namespace ripl.math
          |  $ pi = 3.14159265""".stripMargin
      )(
        "ripl" -> Namespace(
          "math" -> Namespace("pi" -> Cons(Name("$"), VFlt(3.14159265f)))
        )
      )
      testAst(
        """namespace ripl.math.constants
          |  $ pi = 3.14159265""".stripMargin
      )(
        "ripl" -> Namespace(
          "math" -> Namespace(
            "constants" -> Namespace("pi" -> Cons(Name("$"), VFlt(3.14159265f)))
          )
        )
      )
      testAst(
        """namespace ripl.math
          |  namespace constants
          |    $ pi = 3.14159265""".stripMargin
      )(
        "ripl" -> Namespace(
          "math" -> Namespace(
            "constants" -> Namespace("pi" -> Cons(Name("$"), VFlt(3.14159265f)))
          )
        )
      )
    }
  }
}
