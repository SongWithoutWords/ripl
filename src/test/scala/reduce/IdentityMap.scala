package reduce

import org.scalatest._

import ast.untyped._

class TestIdentityMap extends FreeSpec with Matchers {
  "An IdentityMap" - {
    "should find objects by reference" in {
      val map = new IdentityMap[Exp, String]
      val a = VInt(4)
      map.put(a, "a")
      map.get(a) shouldBe Some("a")
    }
    "should not find objects by value" in {
      val map = new IdentityMap[Exp, String]
      val a = VInt(4)
      val b = VInt(4)
      map.put(a, "a")
      map.get(b) shouldBe None
    }
  }
}
