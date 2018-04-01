package reduce

import scala.collection.mutable.Set
import scala.collection.mutable.Stack

import reduce.ast.common._
import reduce.ast.{untyped => a0}
import reduce.ast.{typed => a1}
import reduce.util.MultiMap


object Reduce {

  type Errors = Set[Error]

  def apply(ast: a0.Ast): (a1.Ast, Errors) = {
    val reduce = new Reduce(ast)
    (reduce.astOut, reduce.errors)
  }
}

class Reduce(val astIn: a0.Ast) {
  val errors = Set[Error]()
  def raise(e: Error) = errors += e

  val history = new Stack[a0.Node]
  def historyContains(n: a0.Node) = history.filter(n.eq(_)).nonEmpty
  def catchCycles(input: a0.Node, mapping: (a0.Node) => a1.Node): a1.Node =
    if (historyContains(input)) {
      raise(RecursiveVariableDef(input))
      a1.InvalidExp // Could provide additional information in future
    } else {
      history.push(input)
      val result = mapping(input)
      history.pop()
      result
    }

  val nodes = new IdentityMap[a0.Node, a1.Node]

  val astOut = astIn.mapValues(mapNode)
  val intrinsics = a1.Intrinsic.values.map(i => (i.n, i)).toMap

  type Scope = MultiMap[String, a1.Node]
  var scopes: List[Scope] = Nil

  def pushScope() = scopes = MultiMap[String, a1.Node]() :: scopes
  def pushScope(s: Scope) = scopes = s :: scopes
  def popScope() = scopes = scopes.tail

  def addLocalBinding(n: String, v: a1.Node)
    = scopes = scopes.head.add(n, v) :: scopes.tail

  def lookupName(n: String): List[a1.Node] =
    astOut.get(n).toList ++
      intrinsics.get(n) ++
      scopes.flatMap(_.get(n))

  def asType(node: a1.Node): a1.Type = node match {
    case t: a1.Type => t
    case n => raise(RequiredType(n)); TError
  }
  def mapAsType(node: a0.Node): a1.Type = asType(mapNode(node))
  def asExp(node: a1.Node): a1.Exp = node match {
    case e: a1.Exp => e
    case n => raise(RequiredExp(n)); a1.InvalidExp
  }
  def mapAsExp(node: a0.Node): a1.Exp = asExp(mapNode(node))

  def mapNode(n: a0.Node): a1.Node = nodes.getOrElseUpdate(n, {
    catchCycles(n, (n: a0.Node) => n match {
      case e: a0.Exp => mapExp(e)
      case a0.Namespace(_nodes) =>
        val nodes = _nodes.mapValues(mapNode)
        pushScope(nodes)
        nodes.map.view.force
        popScope()
        a1.Namespace(nodes)
      case t: a0.Type => mapType(t)
    })
  })

  def mapExp(exp: a0.Exp): a1.Node = exp match {

    case a0.App(_f, _args) =>

      val app = a1.App(
        mapAsExp(_f),
        _args.map(mapAsExp))

      (app.f, app.args) match {

        // Compile time evaluation
        case (a1.Intrinsic.IAdd, List(VInt(a), VInt(b))) => VInt(a + b)


        // Method call syntax
        // case (a1.Select(e, n), args) =>
        //   lookupName(n) match {
        //     case method: Exp => method.t match {
        //       case TFun(params, args)
        //   }
        // }

        // Typical function application
        case (f: a1.Exp, args) => f.t match {
          case a1.TFun(params, ret) =>
            if (params.length != args.length) {
              raise(WrongNumArgs(params.length, args.length))
            }
            (params, args).zipped.map((p, a) => constrain(p, a))
            app
          case _ => raise(ApplicationOfNonAppliableType(f.t)); app
        }
      }

    case a0.Block(_exps @ _*) =>
      // TODO: reduce to single exp if is single exp
      pushScope()
      val exps = _exps.map(mapAsExp)
      popScope()
      a1.Block(exps: _*)

    case a0.Cons(_t, _e) =>
      val t = mapAsType(_t)
      val e = mapAsExp(_e)
      constrain(t, e)
      a1.Cons(t, e)

    case a0.If(_a, _b, _c) =>
      val a = mapAsExp(_a)
      constrain(TBln, a)
      a match {
        case v: a1.Val => v match {
          case VBln(true) => mapNode(_b)
          case VBln(false) => mapNode(_c)
          case _ => a1.InvalidExp // Error already emitted by constraint
        }
        case e =>
          val b = mapAsExp(_b)
          val c = mapAsExp(_c)
          constrain(b, c)
          a1.If(a, b, c)
      }

    case a0.Fun(_params, _retType, _body) =>

      // push new scope with the params
      // traverse body
      // either enforce the known return type
      // or gather and find supertype of types returned

      val params = _params.map(p => a1.Param(p.n, mapType(p.t)))

      pushScope(MultiMap(params.map(p => (p.n, p)): _*))
      val body = mapAsExp(_body)
      popScope()

      a1.Fun(params, _retType match {case Some(t) => mapType(t); case _ => TError}, body)

    case a0.Name(n) => lookupName(n) match {
      case Nil => raise(UnknownName(n)); a1.Name(n)
      case x::Nil => x match {
        case n: a1.Namespace => n
        case i: a1.Intrinsic => i
        case v: a1.Val => v
        case t: a1.Type => t
        case e: a1.Exp => a1.Name(n, e)
      }
    }

    case a0.Select(_e, memberName) =>
      val e = mapNode(_e)
      e match {

        // TODO: This is not entirely right, this should be like overload resolution
        case a1.Namespace(units) => units.get(memberName).head

        case a1.VObj(typ, members) => members.get(memberName) match {
          case _::_::_ => ???
          case v::Nil => v
          case Nil => raise(NonExistentMember(memberName)); e
        }

        case e: a1.Exp => e.t match {
          case a1.Struct(_, members) => members.get(memberName) match {
            case _::_::_ => ???
            case t::Nil => a1.Select(e, memberName)
            case Nil => raise(NonExistentMember(memberName)); e
          }
          case TError => throw new Exception(s"$e")
        }
      }

    case a0.Var(n, _e) =>
      val e = mapAsExp(_e)
      addLocalBinding(n, e)
      a1.Var(n, e)

    case v: a0.Val => mapVal(v)
  }

  def mapVal(v: a0.Val): a1.Val = v match {
    case a0.VObj(typ, members) => a1.VObj(mapType(typ), members.mapValues(mapVal))
    case v: ValAtom => v
  }

  def mapType(t: a0.Type): a1.Type = t match {
    case a: TypeAtom => a
    case a0.TFun(_params, _ret) => a1.TFun(_params.map(mapType), mapType(_ret))
    case a0.Struct(name, fields) => a1.Struct(name, fields.mapValues(mapType))
  }

  def constrain(a: a1.Exp, b: a1.Exp): scala.Unit = constrain(a.t, b.t)
  def constrain(a: a1.Type, b: a1.Exp): scala.Unit = constrain(a, b.t)
  def constrain(a: a1.Type, b: a1.Type): scala.Unit = if (a != b) raise(TypeConflict(a, b))

  astOut.map.view.force
}

