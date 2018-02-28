package reduce

import org.scalatest._

import util.{MultiMap => Multi}

class TestReduce extends FreeSpec with Matchers {

  def test(in: Multi[String, Unit])(out: Multi[String, Unit])(errs: Error*): scala.Unit
    = Reduce(in).shouldBe((out, Set(errs: _*)))

  def test(in: (String, Unit)*)(out: (String, Unit)*)(errs: Error*): scala.Unit
    = test(Multi(in: _*))(Multi(out: _*))(errs: _*)

  def testErrs(in: Multi[String, Unit])(errs: Error*): scala.Unit
    = Reduce(in)._2.shouldBe(Set(errs: _*))

  "constants" - {
    "4 + 5 is 9" in {
      val input = "a" -> App(Name("+"), VInt(4), VInt(5))
      val output = "a" -> VInt(9)
      test(input)(output)()
    }
    "a + b is 9 given a = 4 and b = 5" in {
      val input = Multi(
        "a" -> VInt(4),
        "b" -> VInt(5),
        "c" -> App(Name("+"), Name("a"), Name("b")))
      val output = Multi(
        "a" -> VInt(4),
        "b" -> VInt(5),
        "c" -> VInt(9))
      test(input)(output)()
    }
  }
  "named references" - {
    "are found" in {
      val input = Multi("a" -> VInt(4), "b" -> Name("a"))
      val output = Multi("a" -> VInt(4), "b" -> VInt(4))
      test(input)(output)()
    }
    "are found in any order" in {
      val input = Multi("a" -> Name("b"), "b" -> VInt(4))
      val output = Multi("a" -> VInt(4), "b" -> VInt(4))
      test(input)(output)()
    }
    "produce errors when they don't exist" in {
      val input = Multi("a" -> Name("b"))
      test(input)(input)(UnknownName("b"))
    }
    "produce errors when they form cycles" - {
      "at depth 0" in {
        val input = Multi("a" -> Name("a"))
        test(input)(input)(RecursiveVariableDef(Name("a")))
      }
      "at depth 1" in {
        val input = Multi("a" -> Name("b"), "b" -> Name("a"))
        testErrs(input)(RecursiveVariableDef(Name("b")))
      }
      "at depth 2" in {
        val input = Multi(
          "a" -> Name("b"),
          "b" -> Name("c"),
          "c" -> Name("a"))
        testErrs(input)(RecursiveVariableDef(Name("b")))
      }
    }
  }
  "namespaces" - {
    "are traversed during reduction" in {
      val ast = Multi(
        "math" -> Namespace(
          "a" -> Cons(TBln, VInt(4))))
      test(ast)(ast)(TypeConflict(TBln, TInt))
    }
  }
  "type constraints" - {
    "produce no errors when they are met" in {
      val ast = Multi("x" -> Cons(TInt, VInt(3)))
      test(ast)(ast)()
    }
    "produce errors when they are not met" in {
      val ast = Multi("x" -> Cons(TInt, VBln(true)))
      test(ast)(ast)(TypeConflict(TInt, TBln))
    }
  }

  "assignment" - {
  }
  "functions" - {
    "with one parameter" - {
      "bind parameter in body" in {
        def id(ret: Exp) = Fun(Param("a", TInt))(Some(TInt))(ret)

        val identity = id(Name("a"))
        val identityPrime = id(Name("a", Param("a", TInt)))

        test("identity" -> identity)("identity" -> identityPrime)()
      }
      "bind parameter in deep exp in body" in {
        val inc = Fun(Param("a", TInt))(Some(TInt))(
          App(Name("+"), Name("a"), VInt(1)))

        val incPrime = Fun(Param("a", TInt))(Some(TInt))(
          App(Name("+", Intrinsic.IAdd), Name("a", Param("a", TInt)), VInt(1)))

        test("inc" -> inc)("inc" -> incPrime)()
      }
    }

    "with two parameters" - {

      val add = Fun(Param("a", TInt), Param("b", TInt))(Some(TInt))(
        App(Name("+"), Name("a"), Name("b")))

      val addPrime = Fun(Param("a", TInt), Param("b", TInt))(Some(TInt))(
        App(Name("+", Intrinsic.IAdd),
            Name("a", Param("a", TInt)),
            Name("b", Param("b", TInt))))

      "bind parameters in body" in {
        test("add" -> add)("add" -> addPrime)()
      }
      "produce no errors when applied to right types" in {
        val x = App(Name("add"), VInt(4), VInt(5))
        val xPrime = App(Name("add", addPrime), VInt(4), VInt(5))
        test("add" -> add, "x" -> x)("add" -> addPrime, "x" -> xPrime)()
      }
      "produce errors when applied with too few args" in {
        val x = App(Name("add"), VInt(4))
        val xPrime = App(Name("add", addPrime), VInt(4))
        test(
          "add" -> add, "x" -> x)(
          "add" -> addPrime, "x" -> xPrime)(
          WrongNumArgs(2, 1))
      }
      "produce errors when applied with too many args" in {
        val x = App(Name("add"), VInt(4), VInt(5), VInt(6))
        val xPrime = App(Name("add", addPrime), VInt(4), VInt(5), VInt(6))
        test(
          "add" -> add, "x" -> x)(
          "add" -> addPrime, "x" -> xPrime)(
          WrongNumArgs(2, 3))
      }
      "produce errors when applied to wrong types" in {
        val x = App(Name("add"), VInt(4), VBln(true))
        val xPrime = App(Name("add", addPrime), VInt(4), VBln(true))
        test(
          "add" -> add, "x" -> x)(
          "add" -> addPrime, "x" -> xPrime)(
          TypeConflict(TInt, TBln))
      }
      "produce errors when non-applicable type applied" in {
        val input = Multi("a" -> App(VInt(4), VInt(5)))
        test(input)(input)(ApplicationOfNonAppliableType(TInt))
      }
    }
  }
  "if exps" - {
    "produce no errors with correct types" in {
      val select = Fun(Param("a", TBln), Param("b", TInt), Param("c", TInt))(
        Some(TInt))(
        If(Name("a"), Name("b"), Name("c")))
      val selectPrime = Fun(Param("a", TBln), Param("b", TInt), Param("c", TInt))(
        Some(TInt))(
          If(
            Name("a", Param("a", TBln)),
            Name("b", Param("b", TInt)),
            Name("c", Param("c", TInt))))
      test("select" -> select)("select" -> selectPrime)()
    }
    "produces error with non-boolean condition" in {
      val select = Fun(Param("a", TInt), Param("b", TInt), Param("c", TInt))(
        Some(TInt))(
          If(Name("a"), Name("b"), Name("c")))
      val selectPrime = Fun(Param("a", TInt), Param("b", TInt), Param("c", TInt))(
        Some(TInt))(
          If(
            Name("a", Param("a", TInt)),
            Name("b", Param("b", TInt)),
            Name("c", Param("c", TInt))))

      test("select" -> select)("select" -> selectPrime)(TypeConflict(TBln, TInt))
    }
    "branches must yield compatible types" in {
      val select = Fun(Param("a", TBln), Param("b", TInt), Param("c", TBln))(
        Some(TInt))(
          If(Name("a"), Name("b"), Name("c")))
      val selectPrime = Fun(Param("a", TBln), Param("b", TInt), Param("c", TBln))(
        Some(TInt))(
          If(
            Name("a", Param("a", TBln)),
            Name("b", Param("b", TInt)),
            Name("c", Param("c", TBln))))

      test("select" -> select)("select" -> selectPrime)(TypeConflict(TInt, TBln))
    }
  }
  "local variables" - {
    "in blocks" - {
      val _block = Block(
        Var("x", VInt(4)),
        Name("x"))
      val block = Block(
        Var("x", VInt(4)),
        VInt(4))
      "are bound correctly" in {
        test("b" -> _block)("b" -> block)()
      }
      "are not bound outside" in {
        test(
          "b" -> _block, "y" -> Name("x"))(
          "b" -> block, "y" -> Name("x"))(
          UnknownName("x"))
      }
    }
    "in functions" - {
      val a = Param("a", TInt)
      val _inc = Fun(a)(Some(TInt))(
        Block(
          Var("result", App(Name("+"), Name("a"), VInt(1))),
          Name("result")))

      val result = App(Name("+", Intrinsic.IAdd), Name("a", a), VInt(1))
      val inc = Fun(a)(Some(TInt))(
        Block(
          Var("result", result),
          Name("result", result)))

      "are bound correctly" in {
        test("inc" -> _inc)("inc" -> inc)()
      }
      "are not bound outside" in {
        test(
          "inc" -> _inc, "res" -> Name("result"))(
          "inc" -> inc, "res" -> Name("result"))(
          UnknownName("result"))
      }
    }
  }
}

