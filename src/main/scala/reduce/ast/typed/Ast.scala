package reduce.ast.typed

import enumeratum._

import reduce.ast.common._
import reduce.ast.{untyped => a0}
import reduce.util.MultiMap

// I feel like I need to work on my hierarchy a little bit:
// what if everything was a node, we got rid of units, and
// most of the heavy lifting was done in map node?
// That way, expression constructs yielding non-expressions
// (such as types or namespaces) will boil off, leaving only
// true expressions behind?

sealed trait Node

sealed trait Exp extends Node { def t: Type }

case object InvalidNode extends Node

object Namespace {
  def apply(nodes: (String, Node)*): Namespace = Namespace(MultiMap(nodes: _*))
}
case class Namespace(nodes: Nodes) extends Node


trait Type extends Node

// Expressions
object App {
  def apply(f: Exp, args: Exp*): App = App(f, args.toList)
}
case class App(f: Exp, args: List[Exp]) extends Exp {
  def t = f.t match {
    case TFun(_, r) => r
    case _ => TError
  }
}
case class Assign(a: Exp, b: Exp) extends Exp {
  def t = TNone
}
object Block { def apply(exps: Exp*): Block = Block(exps.toList) }
case class Block(exps: List[Exp]) extends Exp {
  def t = exps.lastOption match { case None => TNone; case Some(e) => e.t }
}

// Used to represent type constraints on expressions, such as variable type annotations
case class Cons(t: Type, e: Exp) extends Exp
case class If(a: Exp, b: Exp, c: Exp) extends Exp {
  // TODO: make this a bit more sophisticated (find common super type)
  def t = b.t
}

case object InvalidExp extends Exp {
  def t = TError
}

case class RecursiveDef(cycle: List[a0.Node]) extends Exp {
  def t = TError
}

object Fun {
  def apply(params: Param*)(retType: Type)(body: Exp): Fun
    = Fun(params.toList, retType, body)
}
case class Fun(params: List[Param], retType: Type, body: Exp) extends Exp {
  def t = TFun(params.map(_.t), retType)
}

// Should names even exist in the post reduction ast, or should there
// just be literal references between nodes?
object Name { def apply(n: String, exps: Exp*): Name = Name(n, exps.toList) }
case class Name(n: String, exps: List[Exp]) extends Exp {
  def t = exps match {
    case (e: Exp)::Nil => e.t
    case _ => TError
  }
}

// Idea: store name instead of string to easily catch what we're shadowing?
// (same goes for var)
case class Param(n: String, t: Type) extends Exp
case class Select(e: Exp, n: String, t: Type) extends Exp

case class Var(n: String, e: Exp) extends Exp {
  def t = TNone
}

// Intrinsics
sealed trait Intrinsic extends EnumEntry with Exp { val n: String; val t: TFun }
case object Intrinsic extends Enum[Intrinsic] {
  val values = findValues
  val valuesByName = values.map(v => v.n -> v).toMap
  case object IAdd extends Intrinsic { val n = "+"; val t = TFun(TInt, TInt)(TInt) }
  case object ISub extends Intrinsic { val n = "-"; val t = TFun(TInt, TInt)(TInt) }
  case object IMul extends Intrinsic { val n = "*"; val t = TFun(TInt, TInt)(TInt) }
  case object IDiv extends Intrinsic { val n = "//"; val t = TFun(TInt, TInt)(TInt) }
  case object IMod extends Intrinsic { val n = "%"; val t = TFun(TInt, TInt)(TInt) }

  case object IEql extends Intrinsic { val n = "=="; val t = TFun(TInt, TInt)(TBln) }
  case object INeq extends Intrinsic { val n = "/="; val t = TFun(TInt, TInt)(TBln) }

  case object FAdd extends Intrinsic { val n = "+"; val t = TFun(TFlt, TFlt)(TFlt) }
  case object FSub extends Intrinsic { val n = "-"; val t = TFun(TFlt, TFlt)(TFlt) }
  case object FMul extends Intrinsic { val n = "*"; val t = TFun(TFlt, TFlt)(TFlt) }
  case object FDiv extends Intrinsic { val n = "/"; val t = TFun(TFlt, TFlt)(TFlt) }
  case object FMod extends Intrinsic { val n = "%"; val t = TFun(TFlt, TFlt)(TFlt) }

  case object FEql extends Intrinsic { val n = "=="; val t = TFun(TFlt, TFlt)(TBln) }
  case object FNeq extends Intrinsic { val n = "/="; val t = TFun(TFlt, TFlt)(TBln) }

  // Conversions
  case object ItoF extends Intrinsic { val n = "toFloat"; val t = TFun(TInt)(TFlt) }
  case object FtoI extends Intrinsic { val n = "truncateToInteger"; val t = TFun(TFlt)(TInt) }
}


// Values
trait Val extends Exp
object VObj {
  def apply(t: Type, fields: (String, Val)*): VObj
    = VObj(t, MultiMap(fields: _*))
}
case class VObj(t: Type, fields: MultiMap[String, Val]) extends Val

// Composite types
object TFun { def apply(params: Type*)(ret: Type): TFun = TFun(params.toList, ret) }
case class TFun(params: List[Type], ret: Type) extends Type
object Struct {
  def apply(name: String, fields: (String, Type)*): Struct
    = Struct(name, MultiMap(fields: _*))
}
case class Struct(name: String, fields: MultiMap[String, Type]) extends Type


