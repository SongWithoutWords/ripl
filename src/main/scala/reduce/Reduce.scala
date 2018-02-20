package reduce

import scala.collection.mutable.Set
import scala.collection.mutable.Stack

import ast._


object Reduce {

  type Errors = Set[Error]

  def apply(ast: Ast): (Ast, Errors) = {
    val reduce = new Reduce(ast)
    (reduce.astOut, reduce.errors)
  }
}

class Reduce(val astIn: Ast)
{
  val errors = Set[Error]()
  def raise(e: Error) = errors += e

  val history = new Stack[Node]
  def historyContains(n: Node) = history.filter(n.eq(_)).nonEmpty
  def catchCycles[A <: Node](input: A, mapping: (A) => A) =
    if (historyContains(input)) {
    raise(RecursiveVariableDef())
    input
  } else {
    history.push(input)
    val result = mapping(input)
    history.pop()
    result
  }

  val units = new IdentityMap[Unit, Unit]

  val astOut = astIn.mapValues(mapUnit)

  def mapUnit(u: Unit): Unit = units.getOrElseUpdate(u, {
    catchCycles(u, (u: Unit) => u match {
      case Fun(t, params, body) => Fun(t, params, body.map(mapExp))
      case e: Exp => mapExp(e)
    })
  })

  def mapExp(e: Exp): Exp =
  e match {
    case App(Name("+", _), List(VInt(a), VInt(b))) => VInt(a + b)

    case Name(n, nodes) => astOut.get(n) match {
      case None => UnknownName; Name(n, nodes)
      case Some(x) => if (historyContains(x)) {
        raise(RecursiveVariableDef())
        e
      } else x match {
        case v: Val => v
        case _ => Name(n, mapUnit(x) :: nodes)
        // Name(n, mapUnit(x) :: nodes)
      }
    }
    case _ => e
  }

  astOut.view.force
  // (astOut.view.force, errors)
}

