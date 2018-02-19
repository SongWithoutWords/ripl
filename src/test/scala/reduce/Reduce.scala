package reduce

import org.scalatest._

import ast._

class TestReduce extends FreeSpec with Matchers {
  "constants" - {
    "4 + 5 should be 9" in {
      val input = Map("a" -> App(Name("+", Nil), List(VInt(4), VInt(5))))
      val output = Map("a" -> VInt(9))
      Reduce(input) shouldBe (output, Set())
    }
  }
  "node references" - {
    "units should be found by name" in {
      val input = Map("a" -> VInt(4), "b" -> Name("a", Nil))
      val output = Map("a" -> VInt(4), "b" -> Name("a", VInt(4)::Nil))
      Reduce(input) shouldBe (output, Set())
    }
  }
}
