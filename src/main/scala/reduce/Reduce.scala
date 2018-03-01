package reduce

import scala.collection.mutable.Set
import scala.collection.mutable.Stack

import reduce.Aliases._
import reduce.util.MultiMap


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

  type Scope = MultiMap[String, Node]
  var scopes: List[Scope] = Nil

  def pushScope() = scopes = MultiMap[String, Node]() :: scopes
  def pushScope(s: Scope) = scopes = s :: scopes
  def popScope() = scopes = scopes.tail

  def addLocalBinding(n: String, v: Node)
    = scopes = scopes.head.add(n, v) :: scopes.tail

  def lookupName(n: String): List[Node] =
    astOut.get(n).toList ++
      intrinsics.get(n) ++
      scopes.flatMap(_.get(n))


  def mapUnit(u: Unit): Unit = units.getOrElseUpdate(u, {
    catchCycles(u, (u: Unit) => u match {
      case e: Exp => mapExp(e)
      case Namespace(_units) =>
        val units = _units.mapValues(mapUnit)
        pushScope(units)
        units.map.view.force
        popScope()
        Namespace(units)
      case s: Struct => s
    })
  })

  def mapTopLevelExp(exp: Exp) = exp match {
    // TODO: catch useless expressions, bind variables, etc
    case _ => ???
  }

  def mapExp(exp: Exp): Exp = exp match {

    case App(_f, _args) =>
      val app = App(mapExp(_f), _args.map(mapExp))
      app match {
        case App(Name("+", List(Intrinsic.IAdd)), List(VInt(a), VInt(b))) => VInt(a + b)
        case App(f, args) => f.t match {
          case TFun(params, ret) =>
            if (params.length != args.length) {
              raise(WrongNumArgs(params.length, args.length))
            }
            (params, args).zipped.map((p, a) => constrain(p, a))
            app
          case _ => raise(ApplicationOfNonAppliableType(f.t)); app
        }
      }

    case Block(_exps @ _*) =>
      // TODO: reduce to single exp if is single exp
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

      pushScope(MultiMap(params.map(p => (p.n, p)): _*))

      val body = mapExp(_body)
      popScope()

      Fun(params, retType, body)

    case _n: Name =>
      val n = mapName(_n)
      // This is complicating things a bit - might be nicer to have an unwrap method
      n.nodes match {
        case List(v: Val) => v
        case _ => n
      }

    case Select(_e, memberName) =>
      val e = mapExp(_e)
      e match {
        case Name(name, List(Namespace(units))) =>
          Name(s"$name.$memberName", units.get(memberName))
        case VObj(typ, members) => members.get(memberName) match {
          case ms@_::_::_ => println(s"Way to many members $ms"); ???
          case List(v) => v
          case Nil => raise(UnknownName(memberName)); e
        }
      }

    case Var(n, _e) =>
      val e = mapExp(_e)
      addLocalBinding(n, e)
      Var(n, e)

    case e => e
  }

  def mapName(name: Name) = lookupName(name.n) match {
    case Nil => raise(UnknownName(name.n)); name
    case x::Nil => if (historyContains(x)) {
      raise(RecursiveVariableDef(x))
      name
    } else Name(name.n, x :: name.nodes)
  }

  def constrain(a: Exp, b: Exp): scala.Unit = constrain(a.t, b.t)
  def constrain(a: Type, b: Exp): scala.Unit = constrain(a, b.t)
  def constrain(a: Type, b: Type): scala.Unit = if (a != b) raise(TypeConflict(a, b))

  astOut.map.view.force
}

