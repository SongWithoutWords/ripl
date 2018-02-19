package reduce

import org.scalatest._

import ast._

class TestReduce extends FreeSpec with Matchers {
  "constants" - {
    "4 + 5 should be 9" in {
      Reduce().exp(App(Name("+", Nil), List(VInt(4), VInt(5)))) shouldBe VInt(9)
    }
  }
  "node references" - {
    "units should be found by name" in {
      val a = Var("a", VInt(4))
      val b = Var("b", Name("a", Nil))
      val ast = Ast(Map("a" -> a, "b" -> b))

      val bReduced = Var("b", Name("a", List(a)))
      val astReduced = Ast(Map("a" -> a, "b" -> bReduced))

      Reduce(ast).ast(ast) shouldBe astReduced
    }
  }
}
