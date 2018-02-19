package reduce

import scala.collection.mutable.Stack

import ast._

object Reduce {
  def apply(a: Ast) = new Reduce(a)
  def apply() = new Reduce(Ast(Map()))
}

class Reduce(val a: Ast) {

  val history = new Stack[Unit]
  val units = new IdentityMap[Unit, Unit]

  def ast(a: Ast): Ast = {
    Ast(a.units.mapValues(unit))
  }

  def unit(u: Unit): Unit = units.getOrElseUpdate(u, {
    history.push(u)
    val result = u match {
      case Fun(t, params, body) => Fun(t, params, body.map(exp))
      case Var(n, e) => Var(n, exp(e))
    }
    history.pop()
    result
  })

  def exp(e: Exp): Exp = {
    // history.push(e)
    val result = e match {

      case App(Name("+", _), List(VInt(a), VInt(b))) => VInt(a + b)

      case Name(n, nodes) => Name(n, nodes ++ a.units.get(n))

      case _ => e
    }
    // history.pop()
    result
  }

}

