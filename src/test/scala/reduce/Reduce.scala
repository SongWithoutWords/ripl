package reduce

import org.scalatest._

import ast._

class TestReduce extends FreeSpec with Matchers {
  "constants" - {
    "4 + 5 should be 9" in {
      Reduce(Map("a" -> App(Name("+", Nil), List(VInt(4), VInt(5))))) shouldBe
        Map("a" ->VInt(9))
    }
  }
  "node references" - {
    "units should be found by name" in {
      val ast = Map("a" -> VInt(4), "b" -> Name("a", Nil))
      val astReduced = Map("a" -> VInt(4), "b" -> Name("a", VInt(4)::Nil))
      Reduce(ast) shouldBe astReduced
    }
  }
}
