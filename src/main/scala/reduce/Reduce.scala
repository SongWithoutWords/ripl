package reduce

import scala.collection.immutable.Set
import scala.collection.mutable.Stack

// import scalaz._

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

case object ReduceM {

  def impure(impureAction: =>Unit) = {impureAction; pure()}

  def pure[A](a: A): ReduceM[A] = ReduceM(a, Set())

  def raise(e: Error): ReduceM[Unit] = ReduceM((), Set(e))

  def constrain(a: a1.Exp, b: a1.Exp): ReduceM[Unit] = constrain(a.t, b.t)
  def constrain(a: a1.Type, b: a1.Exp): ReduceM[Unit] = constrain(a, b.t)
  def constrain(a: a1.Type, b: a1.Type): ReduceM[Unit] = when (a != b) { raise(TypeConflict(a, b)) }

  def when[A](condition: Boolean)(action: ReduceM[Unit]): ReduceM[Unit] =
    if(condition) action else pure()

  def mapM[A, B](as: List[A])(f: A => ReduceM[B]): ReduceM[List[B]] = as match {
    case Nil => pure(Nil)
    case a::rem => f(a) >>= { b => mapM(rem)(f) >>= {bs => pure(b::bs) } }
  }

}
import ReduceM._


case class ReduceM[+A](a: A, errors: Set[Error]) {

  def map[B](f: A => B) = ReduceM(f(a), errors)

  def >>[B](rhs: ReduceM[B]) = ReduceM(rhs.a, errors union rhs.errors)

  def >>=[B](f: A => ReduceM[B]): ReduceM[B] = {
    val ReduceM(b, rhsErrors) = f(a)
    ReduceM(b, errors union rhsErrors)
  }

  def flatMap[B](f: A => ReduceM[B]): ReduceM[B] = >>=(f)
}

class Reduce(val astIn: a0.Ast) {
  var errors = Set[Error]()
  def raiseImpure(e: Error) = errors = errors + e

  val history = new Stack[a0.Node]
  def historyContains(n: a0.Node) = history.filter(n.eq(_)).nonEmpty
  def catchCycles(input: a0.Node, mapping: (a0.Node) => a1.Node): a1.Node =
    if (historyContains(input)) {
      raiseImpure(RecursiveVariableDef(input))
      a1.InvalidExp // Could provide additional information in future
    } else {
      history.push(input)
      val result = mapping(input)
      history.pop()
      result
    }

  val nodes = new IdentityMap[a0.Node, a1.Node]

  val astOut = astIn.mapValues(mapNode(KAny, _))
  val intrinsics = MultiMap(a1.Intrinsic.values.map(i => (i.n, i)): _*)

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
    case n => raiseImpure(RequiredType(n)); TError
  }
  def mapAsType(node: a0.Node): a1.Type = asType(mapNode(KType, node))

  def asExp(node: a1.Node): ReduceM[a1.Exp] = node match {
    case e: a1.Exp => pure(e)
    case n => raise(RequiredExp(n)) >> pure(a1.InvalidExp)
  }
  def mapAsExp(t: Option[a1.Type], node: a0.Node): ReduceM[a1.Exp] = asExp(mapNode(KExp(t), node))

  sealed trait Kind
  case object KAny extends Kind
  case object KType extends Kind
  case class KExp(t: Option[a1.Type]) extends Kind
  // case class KVal(t: a1.Type) extends Kind

  def mapNode(kind: Kind, n: a0.Node): a1.Node = nodes.getOrElseUpdate(n, {
    catchCycles(n, (n: a0.Node) => n match {
      case _e: a0.Exp =>
        val ReduceM(e, errs) = mapExp(KAny, _e)
        errors = errors union errs
        e

      case a0.Namespace(_nodes) =>
        val nodes = _nodes.mapValues(mapNode(KAny, _))
        pushScope(nodes)
        nodes.map.view.force
        popScope()
        a1.Namespace(nodes)

      case t: a0.Type => mapType(t)
    })
  })

  def mapExp(kind: Kind, exp: a0.Exp): ReduceM[a1.Node] = exp match {

    case a0.App(_f, _args) =>

      // val app = a1.App(
        // mapAsExp(None, _f),
        // _args.map(mapAsExp(None, _)))

      for {
        f <- mapAsExp(None, _f)
        args <- mapM(_args){mapAsExp(None, _)}// _args.map(mapAsExp(None, _))

        val app = a1.App(f, args)
        app <- (f, args) match {

          // Compile time evaluation
          case (a1.Intrinsic.IAdd, List(VInt(a), VInt(b))) => pure(VInt(a + b))


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
              (params, args).zipped.map((p, a) => constrainImpure(p, a))
              when(params.length != args.length){ raise(WrongNumArgs(params.length, args.length)) } >>
                pure(app)
            case _ => raise(ApplicationOfNonAppliableType(f.t)) >> pure(app)
          }
        }
      }
      yield app
        // val app = a1.App(f, args)
        // (app.f, app.args) match {

          // Compile time evaluation
          // case (a1.Intrinsic.IAdd, List(VInt(a), VInt(b))) => VInt(a + b)


            // Method call syntax
            // case (a1.Select(e, n), args) =>
            //   lookupName(n) match {
            //     case method: Exp => method.t match {
            //       case TFun(params, args)
            //   }
            // }

          // Typical function application
      //     case (f: a1.Exp, args) => f.t match {
      //       case a1.TFun(params, ret) =>
      //         (params, args).zipped.map((p, a) => constrain(p, a))
      //         when(params.length != args.length, raise(WrongNumArgs(params.length, args.length))) >>
      //           pure(app)
      //       case _ => raise(ApplicationOfNonAppliableType(f.t)) >> pure(app)
      //     }
      //   }
      // }

    case a0.Block(_exps @ _*) =>
      // TODO: reduce to single exp if is single exp
      pushScope()
      for {
        exps <- mapM(_exps.toList){mapAsExp(None, _)} //_exps.map(mapAsExp(None, _))
      } yield {
        popScope();
        a1.Block(exps: _*)
      }

      // pushScope()
      // val exps = _exps.map(mapAsExp(None, _))
      // popScope()
      // pure(a1.Block(exps: _*))

    case a0.Cons(_t, _e) =>
      val t = mapAsType(_t)

      for { e <- mapAsExp(Some(t), _e); _ <- constrain(t, e) } yield a1.Cons(t, e)
      // val e = mapAsExp(Some(t), _e)
      // constrain(t, e)
      // pure(a1.Cons(t, e))

    case a0.If(_a, _b, _c) => for {
      a <- mapAsExp(Some(TBln), _a)
      _ <- constrain(TBln, a)
      result <- a match {
        case v: a1.Val => v match {
          case VBln(true) => pure(mapNode(kind, _b))
          case VBln(false) => pure(mapNode(kind, _c))
          case _ => pure(a1.InvalidExp) // Error already emitted by constraint
        }
        case e => for {
          b <- mapAsExp(None, _b)
          c <- mapAsExp(Some(b.t), _c)
          _ <- constrain(b, c)
        } yield a1.If(a, b, c)

          // come up with something more sophisticated if you can
          // val b = mapAsExp(None, _b)
          // val c = mapAsExp(Some(b.t), _c)
          // constrain(b, c)
          // pure(a1.If(a, b, c))
      }
   } yield result

      // val a = mapAsExp(Some(TBln), _a)
      // // constrain(TBln, a)
      // a match {
      //   case v: a1.Val => v match {
      //     case VBln(true) => pure(mapNode(kind, _b))
      //     case VBln(false) => pure(mapNode(kind, _c))
      //     case _ => pure(a1.InvalidExp) // Error already emitted by constraint
      //   }
      //   case e =>
      //     // come up with something more sophisticated if you can
      //     val b = mapAsExp(None, _b)
      //     val c = mapAsExp(Some(b.t), _c)
      //     // constrain(b, c)
      //     pure(a1.If(a, b, c))
      // }

    case a0.Fun(_params, _retType, _body) =>

      // push new scope with the params
      // traverse body
      // either enforce the known return type
      // or gather and find supertype of types returned

      val params = _params.map(p => a1.Param(p.n, mapAsType(p.t)))

      pushScope(MultiMap(params.map(p => (p.n, p)): _*))
      for {
        body <- mapAsExp(None, _body)
        _ <- impure(popScope())
      } yield a1.Fun(params, _retType match {case Some(t) => mapAsType(t); case _ => TError}, body)

      // pushScope(MultiMap(params.map(p => (p.n, p)): _*))
      // val body = mapAsExp(None, _body)
      // popScope()

      // pure(a1.Fun(params, _retType match {case Some(t) => mapAsType(t); case _ => TError}, body))

    case a0.Name(n) => lookupName(n) match {
      case Nil => raise(UnknownName(n)) >> pure(a1.Name(n))
      case x::Nil => x match {
        case n: a1.Namespace => pure(n)
        case i: a1.Intrinsic => pure(i)
        case v: a1.Val => pure(v)
        case t: a1.Type => pure(t)
        case e: a1.Exp => pure(a1.Name(n, e))
      }
    }

    case a0.Select(_e, memberName) =>
      val e = mapNode(KAny, _e)
      e match {

        // TODO: Filter to the required kind
        case a1.Namespace(units) => pure(units.get(memberName).head)

        // TODO: Filter to the required kind
        case a1.VObj(typ, members) => members.get(memberName) match {
          case _::_::_ => ???
          case v::Nil => pure(v)
          case Nil => raise(NonExistentMember(memberName)) >> pure(e)
        }

        // TODO: Filter to the required kind
        case e: a1.Exp => e.t match {
          case a1.Struct(_, members) => members.get(memberName) match {
            case _::_::_ => ???
            case t::Nil => pure(a1.Select(e, memberName))
            case Nil => raise(NonExistentMember(memberName)) >> pure(e)
          }
          case TError => throw new Exception(s"$e")
        }
      }

    case a0.Var(n, _e) =>
      for {
        e <- mapAsExp(None, _e)
      } yield {
        addLocalBinding(n, e)
        a1.Var(n, e)
      }
      // mapAsExp(None, _e) >>= { e =>
      // addLocalBinding(n, e)
      // pure(a1.Var(n, e))
      // }

    case v: a0.Val => pure(mapVal(v))
  }

  def mapVal(v: a0.Val): a1.Val = v match {
    case a0.VObj(typ, members) => a1.VObj(mapAsType(typ), members.mapValues(mapVal))
    case v: ValAtom => v
  }

  def mapType(t: a0.Type): a1.Type = t match {
    case a: TypeAtom => a
    case a0.TFun(_params, _ret) => a1.TFun(_params.map(mapAsType), mapAsType(_ret))
    case a0.Struct(name, fields) => a1.Struct(name, fields.mapValues(mapAsType))
  }

  def constrainImpure(a: a1.Exp, b: a1.Exp): scala.Unit = constrainImpure(a.t, b.t)
  def constrainImpure(a: a1.Type, b: a1.Exp): scala.Unit = constrainImpure(a, b.t)
  def constrainImpure(a: a1.Type, b: a1.Type): scala.Unit = if (a != b) raiseImpure(TypeConflict(a, b))

  astOut.map.view.force
}

