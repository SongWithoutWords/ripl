package object ast {
  import scala.collection.Map

  case class Ast(units: Map[String, Unit])

  sealed trait Unit
  // case class Namespace(units: Map[String, Unit])
  case class Fun(t: TFun, params: List[Param], body: List[Exp]) extends Unit
  case class Var(n: Name, v: Exp) extends Unit with Exp {
    def t = TNone()
    def reduce = ???
  }

  case class Rec() extends Unit

  case class Param(n: Name, t: Type)

  sealed trait Exp {
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
  case class If(a: Exp, b: List[Exp], c: List[Exp]) extends Exp {
    def t = ???
  }
  case class Name(n: String) extends Exp {
    def t = ???
  }
  case class Select(e: Exp, n: Name) extends Exp {
    def t = ???
  }

  sealed trait Val // A known value
  case class VBln(b: Boolean) extends Exp with Val {
    def t = TBln()
  }
  case class VInt(i: Int) extends Exp with Val {
    def t = TInt()
  }
  case class VObj(t: Type, fields: Map[String, Val]) extends Exp with Val

  sealed trait Type
  case class TBln() extends Type
  case class TError() extends Type
  case class TInt() extends Type
  case class TFun(params: List[Type], ret: Type) extends Type
  case class TNone() extends Type
}
