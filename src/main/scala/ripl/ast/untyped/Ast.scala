package ripl.ast.untyped

import ripl.util.MultiMap

trait Exp

trait Type extends Exp
trait Named { val name: String } // should probably be a Name eventually

object Namespace {
  def apply(nodes: (String, Exp)*): Namespace = Namespace(MultiMap(nodes: _*))
}
case class Namespace(defintions: MultiMap[String, Exp]) extends Exp

// Expressions
object App {
  def apply(f: Exp, args: Exp*): App = App(f, args.toList)
}
case class App(f: Exp, args: List[Exp]) extends Exp
case class Assign(a: Exp, b: Exp)       extends Exp
case class Define(a: Exp, b: Exp)       extends Exp
object Block { def apply(exps: Exp*): Block = Block(exps.toList) }
case class Block(exps: List[Exp])     extends Exp
case class Cons(t: Exp, e: Exp)       extends Exp
case class If(a: Exp, b: Exp, c: Exp) extends Exp
object Fun {
  def apply(params: Param*)(retType: Option[Exp])(body: Exp): Fun =
    Fun(params.toList, retType, body)
}
case class Fun(params: List[Param], retType: Option[Exp], body: Exp) extends Exp
case class Param(n: String, t: Exp)                                  extends Exp
case class Select(e: Exp, n: String)                                 extends Exp

case class Var(n: String, e: Exp) extends Exp // seems to have been replaced with "define"

// Values
trait Val extends Exp
object VObj {
  def apply(t: Exp, fields: (String, Val)*): VObj =
    VObj(t, MultiMap(fields: _*))
}
case class VObj(t: Exp, fields: MultiMap[String, Val]) extends Val

// Composite types
object TFun {
  def apply(params: Exp*)(ret: Exp): TFun = TFun(params.toList, ret)
}
case class TFun(params: List[Exp], ret: Exp) extends Type
object Struct {
  def apply(name: String, fields: (String, Exp)*): Struct =
    Struct(name, MultiMap(fields: _*))
}

case class Struct(name: String, fields: MultiMap[String, Exp])
    extends Type
    with Named
case class Union(name: String, alternatives: List[Exp]) extends Type with Named
