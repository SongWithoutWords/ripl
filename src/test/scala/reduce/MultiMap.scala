package reduce

import org.scalatest._

import reduce.util.MultiMap

class TestMultiMap extends FreeSpec with Matchers {
  "A MultiMap" - {

    val multi = MultiMap("a" -> 1, "a" -> 2, "b" -> 3, "a" -> 4)
    "can associate multiple elements with a key" in {
      multi.get("a") shouldBe List(4, 2, 1)
    }
    "can associate a single element with a key" in {
      multi.get("b") shouldBe List(3)
    }
    "can associate no elements with a key" in {
      multi.get("c") shouldBe List()
    }
  }
}
