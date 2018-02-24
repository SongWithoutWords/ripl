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
    raise(RecursiveVariableDef(input))
    input
  } else {
    history.push(input)
    val result = mapping(input)
    history.pop()
    result
  }

  val units = new IdentityMap[Unit, Unit]

  val astOut = astIn.mapValues(mapUnit)
  val intrinsics = Intrinsic.values.map(i => (i.n, i)).toMap

  type Scope = Map[String, Node]
  var scopes: List[Scope] = Nil

  def pushScope(s: Scope) = { println(s"Pushing scope $s"); scopes = s :: scopes }
  def popScope() = scopes = scopes.tail

  def lookupName(n: String): List[Node] =
    astOut.get(n).toList ++
      intrinsics.get(n) ++
      scopes.flatMap(_.get(n))


  def mapUnit(u: Unit): Unit = units.getOrElseUpdate(u, {
    catchCycles(u, (u: Unit) => u match {
      case e: Exp => mapExp(e)
    })
  })

  def mapSubExps(exp: Exp) = exp match {
    case App(f, args) => App(mapExp(f), args.map(mapExp))
    case Assign(a, b) => Assign(mapExp(a), mapExp(b))
    case Cons(t, e) => Cons(t, mapExp(e))
    case Name(n, nodes) => Name(n, nodes)
    case Select(e, n) => Select(mapExp(e), n)
    case Var(n, e) => Var(n, mapExp(e))
    case _ => exp
  }

  def mapExp(exp: Exp): Exp = mapSubExps(exp) match {
    case App(Name("+", _), List(VInt(a), VInt(b))) => VInt(a + b)

    case App(f, args) => f.t match {
      case TFun(params, ret) => {
        if (params.length != args.length) {
          raise(WrongNumArgs(params.length, args.length))
        }
        (params, args).zipped.map((p, a) => if (a.t != p) raise(TypeConflict(p, a.t)))
        exp
      }
      case _ => raise(ApplicationOfNonAppliableType(f.t)); exp
    }

    case Fun(params, retType, body) => {

      // push new scope with the params
      // traverse body
      // either enforce the known return type
      // or gather and find supertype of types returned

      pushScope(params.map(p => (p.n, p)).toMap)

      val bodyPrime = body.map(mapExp)
      popScope

      Fun(params, retType, bodyPrime)
    }

    case Name(n, nodes) => lookupName(n) match {
      case Nil => raise(UnknownName(n)); exp
      case x::Nil => if (historyContains(x)) {
        raise(RecursiveVariableDef(x))
        exp
      } else x match {
        case v: Val => v
        case _ => Name(n, x :: nodes)
      }
    }
    case _ => exp
  }

  astOut.view.force
}

