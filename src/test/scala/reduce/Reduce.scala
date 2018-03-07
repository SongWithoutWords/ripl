package reduce

import org.scalatest._

import util.{MultiMap => Multi}

import reduce.ast.{untyped => a0, typed => a1}

class TestReduce extends FreeSpec with Matchers {

  def test(in: Multi[String, a0.Node])(out: Multi[String, a1.Node])(errs: Error*): Unit
    = Reduce(in).shouldBe((out, Set(errs: _*)))

  def test(in: (String, a0.Node)*)(out: (String, a1.Node)*)(errs: Error*): Unit
    = test(Multi(in: _*))(Multi(out: _*))(errs: _*)

  def testErrs(in: Multi[String, a0.Node])(errs: Error*): Unit
    = Reduce(in)._2.shouldBe(Set(errs: _*))

  def testErrs(in: (String, a0.Node)*)(errs: Error*): Unit
    = testErrs(Multi(in: _*))(errs: _*)

  "constants" - {
    "4 + 5 is 9" in {
      val input = "a" -> a0.App(a0.Name("+"), a0.VInt(4), a0.VInt(5))
      val output = "a" -> a1.VInt(9)
      test(input)(output)()
    }
    "a + b is 9 given a = 4 and b = 5" in {
      val input = Multi(
        "a" -> a0.VInt(4),
        "b" -> a0.VInt(5),
        "c" -> a0.App(a0.Name("+"), a0.Name("a"), a0.Name("b")))
      val output = Multi(
        "a" -> a1.VInt(4),
        "b" -> a1.VInt(5),
        "c" -> a1.VInt(9))
      test(input)(output)()
    }
  }
  "named references" - {
    "are found" in {
      test(
        "a" -> a0.VInt(4),
        "b" -> a0.Name("a"))(
        "a" -> a1.VInt(4),
        "b" -> a1.VInt(4))()
    }
    "are found in any order" in {
      test(
        "a" -> a0.Name("b"),
        "b" -> a0.VInt(4))(
        "a" -> a1.VInt(4),
        "b" -> a1.VInt(4))()
    }
    "produce errors when they don't exist" in {
      test("a" -> a0.Name("b"))("a" -> a1.Name("b"))(UnknownName("b"))
    }
    "produce errors when they form cycles" - {
      "at depth 0" in {
        // test("a" -> a0.Name("a"))("a" -> a1.Name("a"))(RecursiveVariableDef(a1.Name("a")))
      }
      "at depth 1" in {
        // val input = Multi("a" -> Name("b"), "b" -> Name("a"))
        // testErrs("a" -> a0.Name("b"), "b" -> a0.Name("a"))(RecursiveVariableDef(a1.Name("b")))
      }
      "at depth 2" in {
        // val input = Multi(
        //   "a" -> Name("b"),
        //   "b" -> Name("c"),
        //   "c" -> Name("a"))
        // testErrs(
        //   "a" -> a0.Name("b"),
        //   "b" -> a0.Name("c"),
        //   "c" -> a0.Name("a"))(
        //   RecursiveVariableDef(a1.Name("b")))
      }
    }
  }
  "namespaces" - {
    "are traversed during reduction" in {
      // val ast = Multi(
      //   "n" -> Namespace(
      //     "a" -> Cons(TBln, VInt(4))))
      test(
        "n" -> a0.Namespace(
          "a" -> a0.Cons(a0.TBln, a0.VInt(4))))(
        "n" -> a1.Namespace(
          "a" -> a1.Cons(a1.TBln, a1.VInt(4))))(
        TypeConflict(a1.TBln, a1.TInt))
    }
    "units are visible within their namespace" in {
      // val _ast = Multi(
      //   "n" -> Namespace(
      //     "a" -> VInt(4),
      //     "b" -> Name("a")))
      // val ast = Multi(
      //   "n" -> Namespace(
      //     "a" -> VInt(4),
      //     "b" -> VInt(4)))
      test(
        "n" -> a0.Namespace(
          "a" -> a0.VInt(4),
          "b" -> a0.Name("a")))(
        "n" -> a1.Namespace(
          "a" -> a1.VInt(4),
          "b" -> a1.VInt(4)))()
    }
    "units are visible from sub-namespaces" in {
      // val _ast = Multi(
      //   "n" -> Namespace(
      //     "a" -> VInt(4),
      //     "m" -> Namespace(
      //       "b" -> Name("a"))))
      // val ast = Multi(
      //   "n" -> Namespace(
      //     "a" -> VInt(4),
      //     "m" -> Namespace(
      //       "b" -> VInt(4))))
      test(
        "n" -> a0.Namespace(
          "a" -> a0.VInt(4),
          "m" -> a0.Namespace(
            "b" -> a0.Name("a"))))(
        "n" -> a1.Namespace(
          "a" -> a1.VInt(4),
          "m" -> a1.Namespace(
            "b" -> a1.VInt(4))))()
    }
    "units are not visible from outer namespaces" in {
      // val _ast = Multi(
      //   "n" -> Namespace(
      //     "a" -> VInt(4)),
      //   "b" -> Name("a"))
      // val ast = Multi(
      //   "n" -> Namespace(
      //     "a" -> VInt(4)),
      //   "b" -> Name("a"))
      test(
        "n" -> a0.Namespace(
          "a" -> a0.VInt(4)),
        "b" -> a0.Name("a"))(
        "n" -> a1.Namespace(
          "a" -> a1.VInt(4)),
        "b" -> a1.Name("a"))(
        UnknownName("a"))
    }
    "units can be selected by name" in {
      // val _ast = Multi(
      //   "n" -> Namespace(
      //     "a" -> VInt(4)),
      //   "b" -> Select(Name("n"), "a"))
      // val ast = Multi(
      //   "n" -> Namespace(
      //     "a" -> VInt(4)),
      //   "b" -> Name("n.a", VInt(4)))
      test(
        "n" -> a0.Namespace(
          "a" -> a0.VInt(4)),
        "b" -> a0.Select(a0.Name("n"), "a"))(
        "n" -> a1.Namespace(
          "a" -> a1.VInt(4)),
        "b" -> a1.Name("n.a", a1.VInt(4)))()
    }
    "units can be selected by name at depth" in {
      // val _ast = Multi(the
      //   "n" -> Namespace(
      //     "a" -> VInt(4),
      //     "m" -> Namespace(
      //       "b" -> VInt(7))),
      //   "c" -> Select(Select(Name("n"), "m"), "b"))
      // val ast = Multi(
      //   "n" -> Namespace(
      //     "a" -> VInt(4),
      //     "m" -> Namespace(
      //       "b" -> VInt(7))),
      //   "c" -> Name("n.m.b", VInt(7)))
      test(
        "n" -> a0.Namespace(
          "a" -> a0.VInt(4),
          "m" -> a0.Namespace(
            "b" -> a0.VInt(7))),
        "c" -> a0.Select(a0.Select(a0.Name("n"), "m"), "b"))(
        "n" -> a1.Namespace(
          "a" -> a1.VInt(4),
          "m" -> a1.Namespace(
            "b" -> a1.VInt(7))),
        "c" -> a1.Name("n.m.b", a1.VInt(7)))()
    }
  }
  "type constraints" - {
    "produce no errors when they are met" in {
      // val ast = Multi("x" -> Cons(TInt, VInt(3)))
      test(
        "x" -> a0.Cons(a0.TInt, a0.VInt(3)))(
        "x" -> a1.Cons(a1.TInt, a1.VInt(3)))()
    }
    "produce errors when they are not met" in {
      // val ast = Multi("x" -> Cons(TInt, VBln(true)))
      test(
        "x" -> a0.Cons(a0.TInt, a0.VBln(true)))(
        "x" -> a1.Cons(a1.TInt, a1.VBln(true)))(
        TypeConflict(a1.TInt, a1.TBln))
    }
  }

  "assignment" - {
  }
  "functions" - {
    "with one parameter" - {
      "bind parameter in body" in {
        // def id(ret: Exp) = Fun(Param("a", TInt))(Some(TInt))(ret)

        // val identity = id(Name("a"))
        // val identityPrime = id(Name("a", Param("a", TInt)))

        test(
          "identity" -> a0.Fun(
            a0.Param("a", a0.TInt))(Some(a0.TInt))(
            a0.Name("a")))(
          "identity" -> a1.Fun(
            a1.Param("a", a1.TInt))(a1.TInt)(
            a1.Name("a", a1.Param("a", a1.TInt))))()
      }
      "bind parameter in deep exp in body" in {
        // val inc = Fun(Param("a", TInt))(Some(TInt))(
        //   App(Name("+"), Name("a"), VInt(1)))

        // val incPrime = Fun(Param("a", TInt))(Some(TInt))(
        //   App(Name("+", Intrinsic.IAdd), Name("a", Param("a", TInt)), VInt(1)))

        test(
          "inc" -> a0.Fun(a0.Param("a", a0.TInt))(Some(a0.TInt))(
            a0.App(
              a0.Name("+"),
              a0.Name("a"),
              a0.VInt(1))))(
          "inc" -> a1.Fun(a1.Param("a", a1.TInt))(a1.TInt)(
            a1.App(
              a1.Name("+", a1.Intrinsic.IAdd),
              a1.Name("a", a1.Param("a", a1.TInt)),
              a1.VInt(1))))()
      }
    }

    "with two parameters" - {
      val add = a0.Fun(a0.Param("a", a0.TInt), a0.Param("b", a0.TInt))(Some(a0.TInt))(
        a0.App(
          a0.Name("+"),
          a0.Name("a"),
          a0.Name("b")))

      val addPrime = a1.Fun(a1.Param("a", a1.TInt), a1.Param("b", a1.TInt))(a1.TInt)(
        a1.App(a1.Name("+", a1.Intrinsic.IAdd),
            a1.Name("a", a1.Param("a", a1.TInt)),
            a1.Name("b", a1.Param("b", a1.TInt))))

      "bind parameters in body" in {
        test("add" -> add)("add" -> addPrime)()
      }
      "produce no errors when applied to right types" in {
        val x = a0.App(a0.Name("add"), a0.VInt(4), a0.VInt(5))
        val xPrime = a1.App(a1.Name("add", addPrime), a1.VInt(4), a1.VInt(5))
        test("add" -> add, "x" -> x)("add" -> addPrime, "x" -> xPrime)()
      }
      "produce errors when applied to too few args" in {
        val x = a0.App(a0.Name("add"), a0.VInt(4))
        val xPrime = a1.App(a1.Name("add", addPrime), a1.VInt(4))
        test(
          "add" -> add, "x" -> x)(
          "add" -> addPrime, "x" -> xPrime)(
          WrongNumArgs(2, 1))
      }
      "produce errors when applied to too many args" in {
        val x = a0.App(a0.Name("add"), a0.VInt(4), a0.VInt(5), a0.VInt(6))
        val xPrime = a1.App(a1.Name("add", addPrime), a1.VInt(4), a1.VInt(5), a1.VInt(6))
        test(
          "add" -> add, "x" -> x)(
          "add" -> addPrime, "x" -> xPrime)(
          WrongNumArgs(2, 3))
      }
      "produce errors when applied to wrong types" in {
        val x = a0.App(a0.Name("add"), a0.VInt(4), a0.VBln(true))
        val xPrime = a1.App(a1.Name("add", addPrime), a1.VInt(4), a1.VBln(true))
        test(
          "add" -> add, "x" -> x)(
          "add" -> addPrime, "x" -> xPrime)(
          TypeConflict(a1.TInt, a1.TBln))
      }
      "produce errors when non-applicable type applied" in {
        // val input = Multi("a" -> App(VInt(4), VInt(5)))
        test(
          "a" -> a0.App(a0.VInt(4), a0.VInt(5)))(
          "a" -> a1.App(a1.VInt(4), a1.VInt(5)))(
          ApplicationOfNonAppliableType(a1.TInt))
      }
    }
  }
  "if exps" - {
    "produce no errors with correct types" in {
      val select = a0.Fun(a0.Param("a", a0.TBln), a0.Param("b", a0.TInt), a0.Param("c", a0.TInt))(
        Some(a0.TInt))(
        a0.If(a0.Name("a"), a0.Name("b"), a0.Name("c")))
      val selectPrime = a1.Fun(a1.Param("a", a1.TBln), a1.Param("b", a1.TInt), a1.Param("c", a1.TInt))(
        a1.TInt)(
          a1.If(
            a1.Name("a", a1.Param("a", a1.TBln)),
            a1.Name("b", a1.Param("b", a1.TInt)),
            a1.Name("c", a1.Param("c", a1.TInt))))
      test("select" -> select)("select" -> selectPrime)()
    }
    "produces error with non-boolean condition" in {
      val select = a0.Fun(a0.Param("a", a0.TInt), a0.Param("b", a0.TInt), a0.Param("c", a0.TInt))(
        Some(a0.TInt))(
          a0.If(a0.Name("a"), a0.Name("b"), a0.Name("c")))
      val selectPrime = a1.Fun(a1.Param("a", a1.TInt), a1.Param("b", a1.TInt), a1.Param("c", a1.TInt))(
        a1.TInt)(
          a1.If(
            a1.Name("a", a1.Param("a", a1.TInt)),
            a1.Name("b", a1.Param("b", a1.TInt)),
            a1.Name("c", a1.Param("c", a1.TInt))))

      test("select" -> select)("select" -> selectPrime)(TypeConflict(a1.TBln, a1.TInt))
    }
    "branches must yield compatible types" in {
      val select = a0.Fun(a0.Param("a", a0.TBln), a0.Param("b", a0.TInt), a0.Param("c", a0.TBln))(
        Some(a0.TInt))(
          a0.If(a0.Name("a"), a0.Name("b"), a0.Name("c")))
      val selectPrime = a1.Fun(a1.Param("a", a1.TBln), a1.Param("b", a1.TInt), a1.Param("c", a1.TBln))(
        a1.TInt)(
          a1.If(
            a1.Name("a", a1.Param("a", a1.TBln)),
            a1.Name("b", a1.Param("b", a1.TInt)),
            a1.Name("c", a1.Param("c", a1.TBln))))

      test("select" -> select)("select" -> selectPrime)(TypeConflict(a1.TInt, a1.TBln))
    }
  }
  "local variables" - {
    "in blocks" - {
      val _block = a0.Block(
        a0.Var("x", a0.VInt(4)),
        a0.Name("x"))
      val block = a1.Block(
        a1.Var("x", a1.VInt(4)),
        a1.VInt(4))
      "are bound correctly" in {
        test("b" -> _block)("b" -> block)()
      }
      "are not bound outside" in {
        test(
          "b" -> _block, "y" -> a0.Name("x"))(
          "b" -> block, "y" -> a1.Name("x"))(
          UnknownName("x"))
      }
    }
    "in functions" - {
      // val a = Param("a", TInt)
      val _inc = a0.Fun(a0.Param("a", a0.TInt))(Some(a0.TInt))(
        a0.Block(
          a0.Var("result", a0.App(a0.Name("+"), a0.Name("a"), a0.VInt(1))),
          a0.Name("result")))

      val result = a1.App(a1.Name("+", a1.Intrinsic.IAdd), a1.Name("a", a1.Param("a", a1.TInt)), a1.VInt(1))
      val inc = a1.Fun(a1.Param("a", a1.TInt))(a1.TInt)(
        a1.Block(
          a1.Var("result", result),
          a1.Name("result", result)))

      "are bound correctly" in {
        test("inc" -> _inc)("inc" -> inc)()
      }
      "are not bound outside" in {
        test(
          "inc" -> _inc, "res" -> a0.Name("result"))(
          "inc" -> inc, "res" -> a1.Name("result"))(
          UnknownName("result"))
      }
    }
    "selection" - {
      "members can be selected from struct values" in {
        val _point = a0.Struct("Point", "x" -> a0.TInt, "y" -> a0.TInt)
        val point = a1.Struct("Point", "x" -> a1.TInt, "y" -> a1.TInt)
        // val _ast = Multi(
        //   "Point" -> point,
        //   "a" -> VObj(point, "x" -> VInt(7), "y" -> VInt(3)),
        //   "b" -> Select(Name("a"), "y")
        // )
        // val ast = Multi(
        //   "Point" -> point,
        //   "a" -> VObj(point, "x" -> VInt(7), "y" -> VInt(3)),
        //   "b" -> VInt(3)
        // )
        test(
          "Point" -> _point,
          "a" -> a0.VObj(_point, "x" -> a0.VInt(7), "y" -> a0.VInt(3)),
          "b" -> a0.Select(a0.Name("a"), "y")
        )(
          "Point" -> point,
          "a" -> a1.VObj(point, "x" -> a1.VInt(7), "y" -> a1.VInt(3)),
          "b" -> a1.VInt(3)
        )()
      }
      "members can  be selected from struct variables" in {
        val _point = a0.Struct("Point", "x" -> a0.TInt, "y" -> a0.TInt)
        val _getX = a0.Fun(a0.Param("point", _point))(Some(_point))(
          a0.Cons(a0.TInt, a0.Select(a0.Name("point"), "x")))

        val point = a1.Struct("Point", "x" -> a1.TInt, "y" -> a1.TInt)
        val getX = a1.Fun(a1.Param("point", point))(point)(
          a1.Cons(a1.TInt, a1.Select(a1.Name("point", a1.Param("point", point)), "x")))
        test("getX" -> _getX)("getX" -> getX)()
      }
    }
  }
}

