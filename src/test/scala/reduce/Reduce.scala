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
      val input = "a" -> App(Name("+", Nil), List(VInt(4), VInt(5)))
      val output = "a" -> VInt(9)
      test(input)(output)()
    }
    "a + b is 9 given a = 4 and b = 5" in {
      val input = "a" -> App(Name("+", Nil), List(VInt(4), VInt(5)))
      val output = "a" -> VInt(9)
      test(input)(output)()
    }
  }
  "named references" - {
    "are found" in {
      val input = Multi("a" -> VInt(4), "b" -> Name("a", Nil))
      val output = Multi("a" -> VInt(4), "b" -> VInt(4))
      test(input)(output)()
    }
    "are found in any order" in {
      val input = Multi("a" -> Name("b", Nil), "b" -> VInt(4))
      val output = Multi("a" -> VInt(4), "b" -> VInt(4))
      test(input)(output)()
    }
    "produce errors when they don't exist" in {
      val input = Multi("a" -> Name("b", Nil))
      val output = Multi("a" -> Name("b", Nil))
      test(input)(output)(UnknownName("b"))
    }
    "produce errors when they form cycles" - {
      "at depth 0" in {
        val input = Multi("a" -> Name("a", Nil))
        testErrs(input)(RecursiveVariableDef(Name("a", Nil)))
      }
      "at depth 1" in {
        val input = Multi("a" -> Name("b", Nil), "b" -> Name("a", Nil))
        testErrs(input)(RecursiveVariableDef(Name("b", Nil)))
      }
      "at depth 2" in {
        val input = Multi(
          "a" -> Name("b", Nil),
          "b" -> Name("c", Nil),
          "c" -> Name("a", Nil))
        testErrs(input)(RecursiveVariableDef(Name("b", Nil)))
      }
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
        def id(ret: Exp) = Fun(Param("a", TInt)::Nil, Some(TInt), ret)

        val identity = id(Name("a", Nil))
        val identityPrime = id(Name("a", Param("a", TInt)::Nil))

        test("identity" -> identity)("identity" -> identityPrime)()
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

        test("inc" -> inc)("inc" -> incPrime)()
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
        test("add" -> add)("add" -> addPrime)()
      }
      "produce no errors when applied to right types" in {
        val x = App(Name("add", Nil), List(VInt(4), VInt(5)))
        val xPrime = App(Name("add", addPrime::Nil), List(VInt(4), VInt(5)))
        test("add" -> add, "x" -> x)("add" -> addPrime, "x" -> xPrime)()
      }
      "produce errors when applied with too few args" in {
        val x = App(Name("add", Nil), VInt(4)::Nil)
        val xPrime = App(Name("add", addPrime::Nil), VInt(4)::Nil)
        test(
          "add" -> add, "x" -> x)(
          "add" -> addPrime, "x" -> xPrime)(
          WrongNumArgs(2, 1))
      }
      "produce errors when applied with too many args" in {
        val x = App(Name("add", Nil), VInt(4)::VInt(5)::VInt(6)::Nil)
        val xPrime = App(Name("add", addPrime::Nil), VInt(4)::VInt(5)::VInt(6)::Nil)
        test(
          "add" -> add, "x" -> x)(
          "add" -> addPrime, "x" -> xPrime)(
          WrongNumArgs(2, 3))
      }
      "produce errors when applied to wrong types" in {
        val x = App(Name("add", Nil), VInt(4)::VBln(true)::Nil)
        val xPrime = App(Name("add", addPrime::Nil), VInt(4)::VBln(true)::Nil)
        test(
          "add" -> add, "x" -> x)(
          "add" -> addPrime, "x" -> xPrime)(
          TypeConflict(TInt, TBln))
      }
      "produce errors when non-applicable type applied" in {
        val input = Multi("a" -> App(VInt(4), List(VInt(5))))
        test(input)(input)(ApplicationOfNonAppliableType(TInt))
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
      test("select" -> select)("select" -> selectPrime)()
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

      test("select" -> select)("select" -> selectPrime)(TypeConflict(TBln, TInt))
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

      test("select" -> select)("select" -> selectPrime)(TypeConflict(TInt, TBln))
    }
  }
  "local variables" - {
    "in blocks" - {
      val _block = Block(
        Var("x", VInt(4)),
        Name("x", Nil))
      val block = Block(
        Var("x", VInt(4)),
        VInt(4))
      "are bound correctly" in {
        test("b" -> _block)("b" -> block)()
      }
      "are not bound outside" in {
        test(
          "b" -> _block, "y" -> Name("x", Nil))(
          "b" -> block, "y" -> Name("x", Nil))(
          UnknownName("x"))
      }
    }
    "in functions" - {
      val a = Param("a", TInt)::Nil
      val _result = App(Name("+", Nil), Name("a", Nil)::VInt(1)::Nil)
      val _inc = Fun(a, Some(TInt),
        Block(
          Var("result", _result),
          Name("result", Nil)))

      val result = App(Name("+", Intrinsic.IAdd::Nil), Name("a", a)::VInt(1)::Nil)
      val inc = Fun(a, Some(TInt),
        Block(
          Var("result", App(Name("+", Intrinsic.IAdd::Nil), Name("a", a)::VInt(1)::Nil)),
          Name("result", result::Nil)))

      "are bound correctly" in {
        test("inc" -> _inc)("inc" -> inc)()
      }
      "are not bound outside" in {
        test(
          "inc" -> _inc, "res" -> Name("result", Nil))(
          "inc" -> inc, "res" -> Name("result", Nil))(
          UnknownName("result"))
      }
    }
  }
}

