package reduce

import scala.collection.mutable.Set
import scala.collection.mutable.Stack

import reduce.Aliases._


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

  def pushScope() = scopes = Map[String, Node]() :: scopes
  def pushScope(s: Scope) = scopes = s :: scopes
  def popScope() = scopes = scopes.tail

  def addLocalBinding(n: String, v: Node)
    = scopes = scopes.head.updated(n, v) :: scopes.tail

  def lookupName(n: String): List[Node] =
    astOut.get(n).toList ++
      intrinsics.get(n) ++
      scopes.flatMap(_.get(n))


  def mapUnit(u: Unit): Unit = units.getOrElseUpdate(u, {
    catchCycles(u, (u: Unit) => u match {
      case e: Exp => mapExp(e)
    })
  })

  def mapTopLevelExp(exp: Exp) = exp match {
    // TODO: catch useless expressions, bind variables, etc
    case _ => ???
  }

  def mapExp(exp: Exp): Exp = exp match {

    case App(Name("+", _), List(VInt(a), VInt(b))) => VInt(a + b)

    case App(_f, _args) =>
      val f = mapExp(_f)
      val args = _args.map(mapExp)
      f.t match {
        case TFun(params, ret) =>
          if (params.length != args.length) {
            raise(WrongNumArgs(params.length, args.length))
          }
          (params, args).zipped.map((p, a) => constrain(p, a))
          App(f, args)
        case _ => raise(ApplicationOfNonAppliableType(f.t)); exp
      }

    case Block(_exps @ _*) =>
      pushScope()
      val exps = _exps.map(mapExp)
      popScope()
      Block(exps: _*)

    case Cons(t, _e) =>
      val e = mapExp(_e)
      constrain(t, e)
      Cons(t, e)

    case If(_a, _b, _c) =>
      val a = mapExp(_a)
      constrain(TBln, a.t)

      // TODO: reduce to single block if condition is known value

      val b = mapExp(_b)
      val c = mapExp(_c)

      constrain(b, c)

      If(a, b, c)

    case Fun(params, retType, _body) =>

      // push new scope with the params
      // traverse body
      // either enforce the known return type
      // or gather and find supertype of types returned

      pushScope(params.map(p => (p.n, p)).toMap)

      val body = mapExp(_body)
      popScope()

      Fun(params, retType, body)

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

    case Var(n, _e) =>
      val e = mapExp(_e)
      addLocalBinding(n, e)
      Var(n, e)

    case _ => exp
  }

  def constrain(a: Exp, b: Exp): scala.Unit = constrain(a.t, b.t)
  def constrain(a: Type, b: Exp): scala.Unit = constrain(a, b.t)
  def constrain(a: Type, b: Type): scala.Unit = if (a != b) raise(TypeConflict(a, b))

  astOut.map.view.force
}

