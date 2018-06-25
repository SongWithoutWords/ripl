package ripl.integration

import java.nio.file.Paths

import org.scalatest._

import ripl.process._

class TestIntegration extends FreeSpec with Matchers {

  def test(name: String, riplSrc: String)(out: Either[Set[Error], Int]): Unit =
    Run(Paths.get("./target/test/llvm-ir/", name + ".ll"), riplSrc) shouldBe out

  test("000-meaning-of-life", """define main
                                |  lambda () 42""".stripMargin)(Right(42))
}
