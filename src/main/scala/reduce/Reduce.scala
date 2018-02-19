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
    def catchCycles[A <: Node](input: A, mapping: (A) => A) =
      if (history.filter(input.eq(_)).nonEmpty) {
      raise(RecursiveVariableDef())
      input
    } else {
      history.push(input)
      val result = mapping(input)
      history.pop()
      result
    }



    // }
    // def pushNode(n: Node) = {
    //   println("Pushed " + n)
    //   history.push(n)
    // }
    // def popNode() = {
    //   println("Popped " + history.top)
    //   history.pop()
    // }
    // def historyContains(n: Node) = {
    //   val nodeFound = history.filter(n.eq(_)).nonEmpty
    //   if (nodeFound) {
    //     println("Found " + n + " in " + history)
    //   }
    //   else {
    //     println("Did not find " + n + " in " + history)
    //   }
    //   nodeFound
    // }

    val units = new IdentityMap[Unit, Unit]

    def mapUnit(u: Unit): Unit = units.getOrElseUpdate(u, {
      // history.push(u)
      // pushNode(u)
      catchCycles(u, (u: Unit) => u match {
        case Fun(t, params, body) => Fun(t, params, body.map(mapExp))
        case e: Exp => mapExp(e)
      })
      // history.pop()
      // popNode()
      // result
    })

    // def mapSubExprs(e: Exp) = e match {
    //   case App(app, args) => mapExp(app, args.map(mapExp))
    //   case Name()
    // }

    def mapExp(e: Exp): Exp =
    // if (historyContains(e)) {
    //   raise(RecursiveVariableDef())
    //   e
    e match {
      case App(Name("+", _), List(VInt(a), VInt(b))) => VInt(a + b)
      case Name(n, nodes) => Name(n, nodes ++ ast.get(n))
      case _ => e
    }

    // def mapExp(e: Exp): Exp = {
    //   if (historyContains(e)) {
    //     raise(RecursiveVariableDef())
    //     return e
    //   }
    //   // We only need to push referenceable exps, i.e. vars
    //   pushNode(e)
    //   val result = mapExpNoPush(e)
    //   popNode()
    //   result
    // }

    (ast.mapValues(mapUnit), errors)
  }
}

