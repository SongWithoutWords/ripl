package reduce

import org.scalatest._

import ast._

class TestReduce extends FreeSpec with Matchers {
  "constants" - {
    "4 + 5 is 9" in {
      val input = Map("a" -> App(Name("+", Nil), List(VInt(4), VInt(5))))
      val output = Map("a" -> VInt(9))
      Reduce(input) shouldBe (output, Set())
    }
    "a + b is 9 given a = 4 and b = 5" in {
      val input = Map("a" -> App(Name("+", Nil), List(VInt(4), VInt(5))))
      val output = Map("a" -> VInt(9))
      Reduce(input) shouldBe (output, Set())
    }
  }
  "named references" - {
    "are found" in {
      val input = Map("a" -> VInt(4), "b" -> Name("a", Nil))
      val output = Map("a" -> VInt(4), "b" -> VInt(4))
      Reduce(input) shouldBe (output, Set())
    }
    "are found in any order" in {
      val input = Map("a" -> Name("b", Nil), "b" -> VInt(4))
      val output = Map("a" -> VInt(4), "b" -> VInt(4))
      Reduce(input) shouldBe (output, Set())
    }
    "produce errors when they don't exist" in {
      val input = Map("a" -> Name("b", Nil))
      val output = Map("a" -> Name("b", Nil))
      Reduce(input) shouldBe (output, Set(UnknownName("b")))
    }
    "produce errors when they form cycles" - {
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
  "type constraints" - {
    "produce no errors when they are met" in {
      val ast = Map("x" -> Cons(TInt, VInt(3)))
      Reduce(ast) shouldBe (ast, Set())
    }
    "produce errors when they are not met" in {
      val ast = Map("x" -> Cons(TInt, VBln(true)))
      Reduce(ast) shouldBe (ast, Set(TypeConflict(TInt, TBln)))
    }
  }

  "assignment" - {
  }
  "functions" - {
    "with one parameter" - {
      "bind parameter in body" in {
        def id(ret: Exp) = Fun(Param("a", TInt)::Nil, Some(TInt), ret)

        val identity = id(Name("a", Nil))
        val identityPrime = id(Name("a", Param("a", TInt)::Nil))

        Reduce(Map("identity" -> identity)) shouldBe (Map("identity" -> identityPrime), Set())
      }
      "bind parameter in deep exp in body" in {
        val inc = Fun(
          List(Param("a", TInt)),
          Some(TInt),
            App(Name("+", Nil),
                List(Name("a", Nil), VInt(1))))

        val incPrime = Fun(
          List(Param("a", TInt)),
          Some(TInt),
            App(Name("+", Intrinsic.IAdd::Nil),
                List(Name("a", Param("a", TInt)::Nil), VInt(1))))

        Reduce(Map("inc" -> inc)) shouldBe (Map("inc" -> incPrime), Set())
      }
    }

    "with two parameters" - {

      val add = Fun(
        List(Param("a", TInt), Param("b", TInt)),
        Some(TInt),
        App(Name("+", Nil), List(Name("a", Nil), Name("b", Nil))))

      val addPrime = Fun(
        List(Param("a", TInt), Param("b", TInt)),
        Some(TInt),
        App(Name("+", Intrinsic.IAdd::Nil),
                 List(
                   Name("a", Param("a", TInt)::Nil),
                   Name("b", Param("b", TInt)::Nil))))

      "bind parameters in body" in {
        Reduce(Map("add" -> add)) shouldBe (Map("add" -> addPrime), Set())
      }
      "produce no errors when applied to right types" in {
        val x = App(Name("add", Nil), List(VInt(4), VInt(5)))
        val xPrime = App(Name("add", addPrime::Nil), List(VInt(4), VInt(5)))
        Reduce(Map("add" -> add, "x" -> x)) shouldBe
          (Map("add" -> addPrime, "x" -> xPrime), Set())
      }
      "produce errors when applied with too few args" in {
        val x = App(Name("add", Nil), VInt(4)::Nil)
        val xPrime = App(Name("add", addPrime::Nil), VInt(4)::Nil)
        Reduce(Map("add" -> add, "x" -> x)) shouldBe
          (Map("add" -> addPrime, "x" -> xPrime), Set(WrongNumArgs(2, 1)))
      }
      "produce errors when applied with too many args" in {
        val x = App(Name("add", Nil), VInt(4)::VInt(5)::VInt(6)::Nil)
        val xPrime = App(Name("add", addPrime::Nil), VInt(4)::VInt(5)::VInt(6)::Nil)
        Reduce(Map("add" -> add, "x" -> x)) shouldBe
          (Map("add" -> addPrime, "x" -> xPrime), Set(WrongNumArgs(2, 3)))
      }
      "produce errors when applied to wrong types" in {
        val x = App(Name("add", Nil), VInt(4)::VBln(true)::Nil)
        val xPrime = App(Name("add", addPrime::Nil), VInt(4)::VBln(true)::Nil)
        Reduce(Map("add" -> add, "x" -> x)) shouldBe
          (Map("add" -> addPrime, "x" -> xPrime), Set(TypeConflict(TInt, TBln)))
      }
      "produce errors when non-applicable type applied" in {
        val input = Map("a" -> App(VInt(4), List(VInt(5))))
        Reduce(input) shouldBe (input, Set(ApplicationOfNonAppliableType(TInt)))
      }
    }
  }
  "if exps" - {
    "produce no errors with correct types" in {
      val select = Fun(
        List(Param("a", TBln), Param("b", TInt), Param("c", TInt)),
        Some(TInt),
          If(Name("a", Nil),
            Name("b", Nil),
            Name("c", Nil)))
      val selectPrime = Fun(
        Param("a", TBln)::Param("b", TInt)::Param("c", TInt)::Nil,
        Some(TInt),
          If(
            Name("a", Param("a", TBln)::Nil),
            Name("b", Param("b", TInt)::Nil),
            Name("c", Param("c", TInt)::Nil)))

      Reduce(Map("select" -> select)) shouldBe (Map("select" -> selectPrime), Set())
    }
    "produces error with non-boolean condition" in {
      val select = Fun(
        List(Param("a", TInt), Param("b", TInt), Param("c", TInt)),
        Some(TInt),
        If(Name("a", Nil),
            Name("b", Nil),
            Name("c", Nil)))
      val selectPrime = Fun(
        Param("a", TInt)::Param("b", TInt)::Param("c", TInt)::Nil,
        Some(TInt),
        If(
          Name("a", Param("a", TInt)::Nil),
          Name("b", Param("b", TInt)::Nil),
          Name("c", Param("c", TInt)::Nil)))

      Reduce(Map("select" -> select)) shouldBe
      (Map("select" -> selectPrime), Set(TypeConflict(TBln, TInt)))
    }
    "branches must yield compatible types" in {
      val select = Fun(
        List(Param("a", TBln), Param("b", TInt), Param("c", TBln)),
        Some(TInt),
        If(Name("a", Nil),
           Name("b", Nil),
           Name("c", Nil)))
      val selectPrime = Fun(
        Param("a", TBln)::Param("b", TInt)::Param("c", TBln)::Nil,
        Some(TInt),
        If(
          Name("a", Param("a", TBln)::Nil),
          Name("b", Param("b", TInt)::Nil),
          Name("c", Param("c", TBln)::Nil)))

      Reduce(Map("select" -> select)) shouldBe
      (Map("select" -> selectPrime), Set(TypeConflict(TInt, TBln)))
    }
  }
}

