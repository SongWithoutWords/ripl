package reduce

import org.scalatest._

import reduce.util.MultiMap

class TestMultiMap extends FreeSpec with Matchers {
  "A MultiMap" - {

    val multi = MultiMap("a" -> 1, "a" -> 2, "b" -> 3, "a" -> 4)
    "can associate multiple elements with a key" in {
      multi.get("a") shouldBe Set(1, 2, 4)
    }
    "can associate a single element with a key" in {
      multi.get("b") shouldBe Set(3)
    }
    "can associate no elements with a key" in {
      multi.get("c") shouldBe Set()
    }
  }
}
