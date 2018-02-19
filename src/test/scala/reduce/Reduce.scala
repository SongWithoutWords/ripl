package reduce

import org.scalatest._

import ast._

class TestReduce extends FreeSpec with Matchers {
  "reduce" - {
    "constants" - {
      "4 + 5 should be 9" in {
        Reduce()(App(Name("+"), List(VInt(4), VInt(5)))) shouldBe VInt(9)
      }
    }
  }
}
