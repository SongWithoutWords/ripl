package ripl.parse.recursive

import org.scalatest._

import ripl.ast.common._
import ripl.ast.untyped._
import ripl.parse.recursive._

import ripl.reduce.CustomMatchers.matchAst

class ParseTest extends FreeSpec with Matchers {

  def test(name: String, input: String)(out: Exp*): Unit = name in {
    Parse(input) should matchAst(out.toList)
  }

  def test(input: String)(out: Exp*): Unit = test(input, input)(out: _*)

  "atoms" - {
    test("0")(VInt(0))
    test("195")(VInt(195))
    test("0.0")(VFlt(0))
    test("1.374")(VFlt(0))
    test("unenclosed-atom")(Name("unenclosed-atom"))
  }

  "s-expressions" - {

    "non-nested" - {
      test("()")(SExp())
      test("(  )")(SExp())
      test("(     )")(SExp())

      test("(0)")(SExp(VInt(0)))
      test("(195)")(SExp(VInt(195)))
      test("(1.374)")(SExp(VFlt(1.374f)))

      test("(single-enclosed-atom)")(SExp(Name("single-enclosed-atom")))

      test("  (  single-enclosed-atom-with-spaces )    ")(
        SExp(Name("single-enclosed-atom-with-spaces"))
      )

      test("(two enclosed-atoms)")(SExp(Name("two"), Name("enclosed-atoms")))

      test("(three enclosed atoms)")(
        SExp(Name("three"), Name("enclosed"), Name("atoms"))
      )

      test("(multiple 0.9 enclosed 1 atoms)")(
        SExp(
          Name("multiple"),
          VFlt(0.9f),
          Name("enclosed"),
          VInt(1),
          Name("atoms")
        )
      )

      test(" (  many  0.9 enclosed 1 atoms 84.7 with spaces)")(
        SExp(
          Name("many"),
          VFlt(0.9f),
          Name("enclosed"),
          VInt(1),
          Name("atoms"),
          VFlt(84.7f),
          Name("with"),
          Name("spaces")
        )
      )
    }

    "nested" - {
      test("(())")(SExp(SExp()))
      test("(()())")(SExp(SExp(), SExp()))
      test("((()))")(SExp(SExp(SExp())))
      test("((())())")(SExp(SExp(SExp()), SExp()))

      test("((1))")(SExp(SExp(VInt(1))))

      test("((1) 2)")(SExp(SExp(VInt(1)), VInt(2)))

      test("(((1) 2) 3)")(SExp(SExp(SExp(VInt(1)), VInt(2)), VInt(3)))

      test("(a (i j (x)) b ((y z) k) c)")(
        SExp(
          Name("a"),
          SExp(Name("i"), Name("j"), SExp(Name("x"))),
          Name("b"),
          SExp(SExp(Name("y"), Name("z")), Name("k")),
          Name("c")
        )
      )
    }
  }

  "unary operators" - {}

}
