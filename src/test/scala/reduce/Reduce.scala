package reduce

import scala.language.implicitConversions

import org.scalatest._

import util.{MultiMap => Multi}

import reduce.ast.common._
import reduce.ast.common.ImplicitConversions._
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
      test(
        "a" -> a0.App(a0.Name("+"), 4, 5))(
        "a" -> 9)()
    }
    "a + b is 9 given a = 4 and b = 5" in {
      test(
        "a" -> 4,
        "b" -> 5,
        "c" -> a0.App(a0.Name("+"), a0.Name("a"), a0.Name("b")))(
        "a" -> 4,
        "b" -> 5,
        "c" -> 9)()
    }
  }
  "named references" - {
    "are found" in {
      test(
        "a" -> 4,
        "b" -> a0.Name("a"))(
        "a" -> 4,
        "b" -> 4)()
    }
    "are found in any order" in {
      test(
        "a" -> a0.Name("b"),
        "b" -> 4)(
        "a" -> 4,
        "b" -> 4)()
    }
    "produce errors when they don't exist" in {
      test("a" -> a0.Name("b"))("a" -> a1.Name("b"))(UnknownName("b"))
    }
    "produce errors when they form cycles" - {
      "at depth 0" in {
        test(
          "a" -> a0.Name("a"))(
          "a" -> a1.Name("a", a1.InvalidExp))(
          RecursiveVariableDef(a0.Name("a")))
      }
      "at depth 1" in {
        test(
          "a" -> a0.Name("b"),
          "b" -> a0.Name("a"))(
          "a" -> a1.Name("b", a1.Name("a", a1.InvalidExp)),
          "b" -> a1.Name("a", a1.InvalidExp))(
          RecursiveVariableDef(a0.Name("b")))
      }
      "at depth 2" in {
        testErrs(
          "a" -> a0.Name("b"),
          "b" -> a0.Name("c"),
          "c" -> a0.Name("a"))(
          RecursiveVariableDef(a0.Name("b")))
      }
    }
  }
  "namespaces" - {
    "are traversed during reduction" in {
      test(
        "n" -> a0.Namespace(
          "a" -> a0.Cons(TBln, 4)))(
        "n" -> a1.Namespace(
          "a" -> a1.Cons(TBln, 4)))(
        TypeConflict(TBln, TInt))
    }
    "units are visible within their namespace" in {
      test(
        "n" -> a0.Namespace(
          "a" -> 4,
          "b" -> a0.Name("a")))(
        "n" -> a1.Namespace(
          "a" -> 4,
          "b" -> 4))()
    }
    "units are visible from sub-namespaces" in {
      test(
        "n" -> a0.Namespace(
          "a" -> 4,
          "m" -> a0.Namespace(
            "b" -> a0.Name("a"))))(
        "n" -> a1.Namespace(
          "a" -> 4,
          "m" -> a1.Namespace(
            "b" -> VInt(4))))()
    }
    "units are not visible from outer namespaces" in {
      test(
        "n" -> a0.Namespace(
          "a" -> VInt(4)),
        "b" -> a0.Name("a"))(
        "n" -> a1.Namespace(
          "a" -> VInt(4)),
        "b" -> a1.Name("a"))(
        UnknownName("a"))
    }
    "units can be selected by name" in {
      test(
        "n" -> a0.Namespace(
          "a" -> VInt(4)),
        "b" -> a0.Select(a0.Name("n"), "a"))(
        "n" -> a1.Namespace(
          "a" -> VInt(4)),
        "b" -> 4)()
    }
    "units can be selected by name at depth" in {
      test(
        "n" -> a0.Namespace(
          "a" -> 4,
          "m" -> a0.Namespace(
            "b" -> VInt(7))),
        "c" -> a0.Select(a0.Select(a0.Name("n"), "m"), "b"))(
        "n" -> a1.Namespace(
          "a" -> 4,
          "m" -> a1.Namespace(
            "b" -> VInt(7))),
        "c" -> 7)()
    }
  }
  "type" - {
    "are mapped correctly" - {
      "TBln" in {
        test("bool" -> TBln)("bool" -> TBln)()
      }
      "(TInt, TInt) -> TBln" in {
        test("f" -> a0.TFun(TInt, TInt)(TBln))("f" -> a1.TFun(TInt, TInt)(TBln))()
      }
    }
  }
  "type constraints" - {
    "produce no errors when they are met" in {
      test(
        "x" -> a0.Cons(TInt, 3))(
        "x" -> a1.Cons(TInt, 3))()
    }
    "produce errors when they are not met" in {
      test(
        "x" -> a0.Cons(TInt, true))(
        "x" -> a1.Cons(TInt, true))(
        TypeConflict(TInt, TBln))
    }
  }

  "assignment" - {
  }
  "functions" - {
    "with one parameter" - {
      "bind parameter in body" in {
        test(
          "identity" -> a0.Fun(
            a0.Param("a", TInt))(Some(TInt))(
            a0.Name("a")))(
          "identity" -> a1.Fun(
            a1.Param("a", TInt))(TInt)(
            a1.Name("a", a1.Param("a", TInt))))()
      }
      "bind parameter in deep exp in body" in {
        test(
          "inc" -> a0.Fun(a0.Param("a", TInt))(Some(TInt))(
            a0.App(
              a0.Name("+"),
              a0.Name("a"),
              1)))(
          "inc" -> a1.Fun(a1.Param("a", TInt))(TInt)(
            a1.App(
              a1.Intrinsic.IAdd,
              a1.Name("a", a1.Param("a", TInt)),
              1)))()
      }
    }

    "with two parameters" - {
      val add = a0.Fun(a0.Param("a", TInt), a0.Param("b", TInt))(Some(TInt))(
        a0.App(
          a0.Name("+"),
          a0.Name("a"),
          a0.Name("b")))

      val addPrime = a1.Fun(a1.Param("a", TInt), a1.Param("b", TInt))(TInt)(
        a1.App(a1.Intrinsic.IAdd,
            a1.Name("a", a1.Param("a", TInt)),
            a1.Name("b", a1.Param("b", TInt))))

      "bind parameters in body" in {
        test("add" -> add)("add" -> addPrime)()
      }
      "produce no errors when applied to right types" in {
        val x = a0.App(a0.Name("add"), 4, 5)
        val xPrime = a1.App(a1.Name("add", addPrime), 4, 5)
        test("add" -> add, "x" -> x)("add" -> addPrime, "x" -> xPrime)()
      }
      "produce errors when applied to too few args" in {
        val x = a0.App(a0.Name("add"), 4)
        val xPrime = a1.App(a1.Name("add", addPrime), 4)
        test(
          "add" -> add, "x" -> x)(
          "add" -> addPrime, "x" -> xPrime)(
          WrongNumArgs(2, 1))
      }
      "produce errors when applied to too many args" in {
        val x = a0.App(a0.Name("add"), 4, 5, 6)
        val xPrime = a1.App(a1.Name("add", addPrime), 4, 5, 6)
        test(
          "add" -> add, "x" -> x)(
          "add" -> addPrime, "x" -> xPrime)(
          WrongNumArgs(2, 3))
      }
      "produce errors when applied to wrong types" in {
        val x = a0.App(a0.Name("add"), 4, true)
        val xPrime = a1.App(a1.Name("add", addPrime), 4, true)
        test(
          "add" -> add, "x" -> x)(
          "add" -> addPrime, "x" -> xPrime)(
          TypeConflict(TInt, TBln))
      }
      "produce errors when non-applicable type applied" in {
        test(
          "a" -> a0.App(4, 5))(
          "a" -> 4)(
          ApplicationOfNonAppliableType(TInt))
      }
    }
  }
  "if exps" - {
    "produce no errors with correct types" in {
      val select = a0.Fun(a0.Param("a", TBln), a0.Param("b", TInt), a0.Param("c", TInt))(
        Some(TInt))(
        a0.If(a0.Name("a"), a0.Name("b"), a0.Name("c")))
      val selectPrime = a1.Fun(a1.Param("a", TBln), a1.Param("b", TInt), a1.Param("c", TInt))(
        TInt)(
          a1.If(
            a1.Name("a", a1.Param("a", TBln)),
            a1.Name("b", a1.Param("b", TInt)),
            a1.Name("c", a1.Param("c", TInt))))
      test("select" -> select)("select" -> selectPrime)()
    }
    "produces error with non-boolean condition" in {
      val select = a0.Fun(a0.Param("a", TInt), a0.Param("b", TInt), a0.Param("c", TInt))(
        Some(TInt))(
          a0.If(a0.Name("a"), a0.Name("b"), a0.Name("c")))
      val selectPrime = a1.Fun(a1.Param("a", TInt), a1.Param("b", TInt), a1.Param("c", TInt))(
        TInt)(
          a1.If(
            a1.Name("a", a1.Param("a", TInt)),
            a1.Name("b", a1.Param("b", TInt)),
            a1.Name("c", a1.Param("c", TInt))))

      test("select" -> select)("select" -> selectPrime)(TypeConflict(TBln, TInt))
    }
    "branches must yield compatible types" in {
      val select = a0.Fun(a0.Param("a", TBln), a0.Param("b", TInt), a0.Param("c", TBln))(
        Some(TInt))(
          a0.If(a0.Name("a"), a0.Name("b"), a0.Name("c")))
      val selectPrime = a1.Fun(a1.Param("a", TBln), a1.Param("b", TInt), a1.Param("c", TBln))(
        TInt)(
          a1.If(
            a1.Name("a", a1.Param("a", TBln)),
            a1.Name("b", a1.Param("b", TInt)),
            a1.Name("c", a1.Param("c", TBln))))

      test("select" -> select)("select" -> selectPrime)(TypeConflict(TInt, TBln))
    }
  }
  "local variables" - {
    "in blocks" - {
      val _block = a0.Block(
        a0.Var("x", 4),
        a0.Name("x"))
      val block = a1.Block(
        a1.Var("x", 4),
        4)
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
      val _inc = a0.Fun(a0.Param("a", TInt))(Some(TInt))(
        a0.Block(
          a0.Var("result", a0.App(a0.Name("+"), a0.Name("a"), 1)),
          a0.Name("result")))

      val result = a1.App(a1.Intrinsic.IAdd, a1.Name("a", a1.Param("a", TInt)), 1)
      val inc = a1.Fun(a1.Param("a", TInt))(TInt)(
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
  }
  "selection" - {
    "members can be selected from struct values" in {
      val _point = a0.Struct("Point", "x" -> TInt, "y" -> TInt)
      val point = a1.Struct("Point", "x" -> TInt, "y" -> TInt)
      test(
        "Point" -> _point,
        "a" -> a0.VObj(a0.Name("Point"), "x" -> 7, "y" -> 3),
        "b" -> a0.Select(a0.Name("a"), "y")
      )(
        "Point" -> point,
        "a" -> a1.VObj(point, "x" -> 7, "y" -> 3),
        "b" -> 3
      )()
    }
    "members can  be selected from struct variables" in {
      val _point = a0.Struct("Point", "x" -> TInt, "y" -> TInt)
      val _getX = a0.Fun(a0.Param("point", _point))(Some(_point))(
        a0.Cons(TInt, a0.Select(a0.Name("point"), "x")))

      val point = a1.Struct("Point", "x" -> TInt, "y" -> TInt)
      val getX = a1.Fun(a1.Param("point", point))(point)(
        a1.Cons(TInt, a1.Select(a1.Name("point", a1.Param("point", point)), "x")))
      test("getX" -> _getX)("getX" -> getX)()
    }
  }

  "overloads" - {
    "integer addition is selected for ints" in {
      test(
        "iAdd" -> a0.Fun(a0.Param("a", TInt), a0.Param("b", TInt))(Some(TInt))(
          a0.App(a0.Name("+"),
                 a0.Name("a"),
                 a0.Name("b"))))(
        "iAdd" -> a1.Fun(a1.Param("a", TInt), a1.Param("b", TInt))(TInt)(
          a1.App(a1.Intrinsic.IAdd,
                 a1.Name("a", a1.Param("a", TInt)),
                 a1.Name("b", a1.Param("b", TInt)))))()
    }
    "floating point addition is selected for floats" in {
      test(
        "iAdd" -> a0.Fun(a0.Param("a", TFlt), a0.Param("b", TFlt))(Some(TFlt))(
          a0.App(a0.Name("+"),
                 a0.Name("a"),
                 a0.Name("b"))))(
        "iAdd" -> a1.Fun(a1.Param("a", TFlt), a1.Param("b", TFlt))(TFlt)(
          a1.App(a1.Intrinsic.FAdd,
                 a1.Name("a", a1.Param("a", TFlt)),
                 a1.Name("b", a1.Param("b", TFlt)))))()
    }
  }
}

