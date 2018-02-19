package reduce

import scala.collection.mutable.Set
import scala.collection.mutable.Stack

import ast._


object Reduce {

  type Errors = Set[Error]

  def apply(ast: Ast): (Ast, Errors) = {

    val errors = Set[Error]()
    def raise(e: Error) = errors += e

    val history = new Stack[Node]
    val units = new IdentityMap[Unit, Unit]

    def redUnit(u: Unit): Unit = units.getOrElseUpdate(u, {
      history.push(u)
      val result = u match {
        case Fun(t, params, body) => Fun(t, params, body.map(redExp))
        case e: Exp => redExp(e)
      }
      history.pop()
      result
    })

    def redExp(e: Exp): Exp = {
      if (history.contains(e)) {

      }

      history.push(e)
      val result = e match {

        case App(Name("+", _), List(VInt(a), VInt(b))) => VInt(a + b)

        case Name(n, nodes) => Name(n, nodes ++ ast.get(n))

        case _ => e
      }
      history.pop()
      result
    }

    (ast.mapValues(redUnit), errors)
  }
}

