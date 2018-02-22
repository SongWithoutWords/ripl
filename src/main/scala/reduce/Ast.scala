package object ast {
  import scala.collection.immutable.Map

  // case class Ast(units: Units)
  type Ast = Units

  // A named and referenceable node within the Ast
  sealed trait Node

  type Units = Map[String, Unit]
  sealed trait Unit extends Node
  // case class Namespace(units: Map[String, Unit])

  case class Fun(params: List[Param], retType: Option[Type], body: List[Exp])
      extends Unit with Exp {
    def t = retType match {
      case Some(tRet) => TFun(params.map(_.t), tRet)
      case None => TError()
    }
  }
  // case class Rec() extends Unit

  // Idea: store name instead of string to easily catch what we're shadowing?
  // (same goes for var)
  case class Param(n: String, t: Type) extends Node

  sealed trait Exp extends Unit {
    def t: Type
  }
  case class App(f: Exp, args: List[Exp]) extends Exp {
    def t = f.t match {
      case TFun(_, r) => r
      case _ => TError()
    }
  }
  case class Assign(a: Exp, b: Exp) extends Exp {
    def t = TNone()
  }
  // Used to represent type constraints on expressions, such as variable type annotations
  case class Cons(t: Type, a: Exp) extends Exp
  case class If(a: Exp, b: List[Exp], c: List[Exp]) extends Exp {
    def t = ???
  }
  case class Name(n: String, nodes: List[Node]) extends Exp {
    def t = nodes match {
      case (e: Exp)::Nil => e.t
      case (f: Fun)::Nil => f.t
      case _ => println("foop: " + n); TError()
    }
  }
  case class Select(e: Exp, n: Name) extends Exp {
    def t = ???
  }
  case class Var(n: String, e: Exp) extends Unit with Exp {
    def t = TNone()
  }

  sealed trait Val extends Exp // A known value
  case class VBln(b: Boolean) extends Val {
    def t = TBln()
  }
  case class VInt(i: Int) extends Val {
    def t = TInt()
  }
  case class VObj(t: Type, fields: Map[String, Val]) extends Val

  sealed trait Type
  case class TBln() extends Type
  case class TError() extends Type
  case class TInt() extends Type
  case class TFun(params: List[Type], ret: Type) extends Type
  case class TNone() extends Type
}
