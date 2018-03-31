package reduce.ast.untyped

import reduce.util.MultiMap

sealed trait Node
sealed trait Exp extends Node
object Namespace {
  def apply(nodes: (String, Node)*): Namespace = Namespace(MultiMap(nodes: _*))
}
case class Namespace(nodes: Nodes) extends Node
trait Type extends Node

// Expressions
object App {
  def apply(f: Node, args: Node*): App = App(f, args.toList)
}
case class App(f: Node, args: List[Node]) extends Exp
case class Assign(a: Exp, b: Exp) extends Exp
case class Block(exps: Exp*) extends Exp
case class Cons(t: Type, e: Exp) extends Exp
case class If(a: Exp, b: Exp, c: Exp) extends Exp
object Fun {
  def apply(params: Param*)(retType: Option[Type])(body: Exp): Fun
    = Fun(params.toList, retType, body)
}
case class Fun(params: List[Param], retType: Option[Type], body: Exp) extends Exp
case class Name(n: String) extends Exp
case class Param(n: String, t: Type) extends Exp
case class Select(e: Exp, n: String) extends Exp
case class Var(n: String, e: Exp) extends Exp

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
