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
    "a + b should be 9 given a = 4 and b = 5" in {
      val input = Map("a" -> App(Name("+", Nil), List(VInt(4), VInt(5))))
      val output = Map("a" -> VInt(9))
      Reduce(input) shouldBe (output, Set())
    }
  }
  "variables" - {
    "variables should be found by name" in {
      val input = Map("a" -> VInt(4), "b" -> Name("a", Nil))
      val output = Map("a" -> VInt(4), "b" -> VInt(4))
      Reduce(input) shouldBe (output, Set())
    }
    "variables should be found in any order" in {
      val input = Map("a" -> Name("b", Nil), "b" -> VInt(4))
      val output = Map("a" -> VInt(4), "b" -> VInt(4))
      Reduce(input) shouldBe (output, Set())
    }
    "cycles should be detected" - {
      "at depth 0" in {
        val input = Map("a" -> Name("a", Nil))
        Reduce(input)._2 shouldBe Set(RecursiveVariableDef())
      }
      "at depth 1" in {
        val input = Map("a" -> Name("b", Nil), "b" -> Name("a", Nil))
        Reduce(input)._2 shouldBe Set(RecursiveVariableDef())
      }
      "at depth 2" in {
        val input = Map(
          "a" -> Name("b", Nil),
          "b" -> Name("c", Nil),
          "c" -> Name("a", Nil))
        Reduce(input)._2 shouldBe Set(RecursiveVariableDef())
      }
    }
  }
}
