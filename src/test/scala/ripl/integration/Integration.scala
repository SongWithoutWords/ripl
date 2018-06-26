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
}
