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
  "variable references" - {
    "should be found by name" in {
      val input = Map("a" -> VInt(4), "b" -> Name("a", Nil))
      val output = Map("a" -> VInt(4), "b" -> VInt(4))
      Reduce(input) shouldBe (output, Set())
    }
    "should be found in any order" in {
      val input = Map("a" -> Name("b", Nil), "b" -> VInt(4))
      val output = Map("a" -> VInt(4), "b" -> VInt(4))
      Reduce(input) shouldBe (output, Set())
    }
    "should result in errors when they don't exist" in {
      val input = Map("a" -> Name("b", Nil))
      val output = Map("a" -> Name("b", Nil))
      Reduce(input) shouldBe (output, Set(UnknownName("b")))
    }
    "should result in errors when they form cycles" - {
      "at depth 0" in {
        val input = Map("a" -> Name("a", Nil))
        Reduce(input)._2 shouldBe Set(RecursiveVariableDef)
      }
      "at depth 1" in {
        val input = Map("a" -> Name("b", Nil), "b" -> Name("a", Nil))
        Reduce(input)._2 shouldBe Set(RecursiveVariableDef)
      }
      "at depth 2" in {
        val input = Map(
          "a" -> Name("b", Nil),
          "b" -> Name("c", Nil),
          "c" -> Name("a", Nil))
        Reduce(input)._2 shouldBe Set(RecursiveVariableDef)
      }
    }
  }
  "basic type checking" - {
    // "should not result in errors when applied to right types" in {
    //   val add = Fun
    //   ( List(Param("a", TInt), Param("b", TInt))
    //      , Some(TInt)
    //      , List(App(Name("+", Nil), "a", "b"))
    //   )
    //   // val input = Map("a" -> VInt(4), "b" -> Name("a", Nil))
    //   // val output = Map("a" -> VInt(4), "b" -> VInt(4))
    //   Reduce(input) shouldBe (output, Set())
    // }
  }
  "functions" - {
    "should produce no errors when applied to right types" in {
      val add = Fun(
        List(Param("a", TInt), Param("b", TInt)),
        Some(TInt),
        List(App(Name("+", Nil), List(Name("a", Nil), Name("b", Nil)))))
      val input = Map("add" -> add, "x" -> App(Name("add", Nil), List(VInt(4), VInt(5))))
      Reduce(input) shouldBe (input, Set())
    }
    "should produce errors when applied to too few args" in {
      val add = Fun(
        List(Param("a", TInt), Param("b", TInt)),
        Some(TInt),
        List(VInt(4)))
      val input = Map("add" -> add, "a" -> App(Name("add", Nil), List(VInt(4))))
      Reduce(input) shouldBe (input, Set(WrongNumArgs(2, 1)))
    }
    "should produce errors when applied to too many args" in {
      val add = Fun(
        List(Param("a", TInt), Param("b", TInt)),
        Some(TInt),
        List(VInt(4)))
      val input = Map("add" -> add, "a" -> App(Name("add", Nil), List(VInt(4), VInt(5), VInt(6))))
      Reduce(input) shouldBe (input, Set(WrongNumArgs(2, 3)))
    }
    "should produce errors when applied to wrong types" in {
      val add = Fun(
        List(Param("a", TInt), Param("b", TInt)),
        Some(TInt),
        List(VInt(4)))
      val input = Map("add" -> add, "a" -> App(add, List(VInt(5), VBln(true))))
      Reduce(input) shouldBe (input, Set(TypeConflict(TInt, TBln)))
    }
    "should produce errors when non-applicable type applied" in {
      val input = Map("a" -> App(VInt(4), List(VInt(5))))
      Reduce(input) shouldBe (input, Set(ApplicationOfNonAppliableType(TInt)))
    }
  }
}

