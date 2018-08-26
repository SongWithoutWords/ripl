package ripl.integration

import java.nio.file.Paths

import org.scalatest._

import ripl.process._

class TestIntegration extends FreeSpec with Matchers {

  def test(name: String, riplSrc: String)(out: Either[Set[Error], Int]): Unit =
    Run(Paths.get("./target/test/llvm-ir/", name + ".ll"), riplSrc) shouldBe out

  test("lambda-style-main", """define main
                              |  lambda () 42""".stripMargin)(Right(42))

  test("function-style-main", """define (main) 42""".stripMargin)(Right(42))

  test(
    "simple-function-call",
    """define (main) (meaning-of-life)
      |define (meaning-of-life) 42""".stripMargin
  )(Right(42))

  test(
    "simple-add",
    """define (main) (add 37 5)
      |define (add (Int a) (Int b)) (+ a b)""".stripMargin
  )(Right(42))

  test(
    "nested-exps",
    """define (main) (multiply-add 8 5 2)
      |define (multiply-add (Int a) (Int b) (Int c))
      |  + (* a b) c
      """.stripMargin
  )(Right(42))

  "if-expressions" - {

    test(
      "ternary-using-if-true",
      """define (main) (ternary true 42 7)
        |define (ternary (Bln a) (Int b) (Int c))
        |  if a b c
        """.stripMargin
    )(Right(42))

    test(
      "ternary-using-if-false",
      """define (main) (ternary false 42 7)
        |define (ternary (Bln a) (Int b) (Int c))
        |  if a b c
        """.stripMargin
    )(Right(7))

    test(
      "cascading-if",
      """define (main) (cascading-if false 7 true 8 9)
        |define (cascading-if (Bln c1) (Int e1) (Bln c2) (Int e2) (Int e3))
        |  if c1 e1 (if c2 e2 e3)
        """.stripMargin
    )(Right(8))

    "xor-using-if-exps" - {

      // Although it doesn't make any practical sense to define by branching,
      // it's a good way to test the generation of nested if-expressions
      def xorUsingIf(a: Boolean, b: Boolean) =
        s"""define (main) (xor-using-if-exps ${a.toString} ${b.toString})
           |define (xor-using-if-exps (Bln a) (Bln b))
           |  if a (if b false true) (if b true false)""".stripMargin

      test(
        "xor-using-if-false-false",
        xorUsingIf(false, false)
      )(Right(0))

      test(
        "xor-using-if-exps-fase-true",
        xorUsingIf(false, true)
      )(Right(1))

      test(
        "xor-using-if-exps-true-false",
        xorUsingIf(true, false)
      )(Right(1))

      test(
        "xor-using-if-exps-true-true",
        xorUsingIf(true, true)
      )(Right(0))
    }
  }

  "structs" - {
    test(
      "pair-sum",
      """define (main) (pair-sum (pair 4 9))
        |struct pair
        |  Int a
        |  Int b
        |define (pair-sum (pair p))
        |  + p.a p.b""".stripMargin
    )(Right(13))
  }

  test(
    "factorial",
    """define (main) (fact 4)
      |
      |define (fact (Int n)) Int
      |  if (<= n 1)
      |    1
      |    * n (fact (- n 1))
      |""".stripMargin
  )(Right(24))

  test(
    "% 13 5",
    "define (main) (% 13 5)"
  )(Right(3))

  test(
    "multiples-of-3-and-5",
    """define (main) (multiples-of-3-and-5 9)
      |
      |define (multiples-of-3-and-5 (Int n)) Int
      |  if (<= n 0)
      |    0
      |    +
      |      if (or (== 0 (% n 3)) (== 0 (% n 5)))
      |        n
      |        0
      |      multiples-of-3-and-5 (- n 1)
      |""".stripMargin
  )(Right(23))

  test(
    "putchar",
    """define (main) (putchar 42)
      |
      |external (putchar (i8 c)) None
      |""".stripMargin
  )(Right(24))
}
