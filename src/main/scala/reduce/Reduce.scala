package reduce

import scala.collection.immutable.Set
import scala.collection.mutable.Stack

import reduce.ast.common._
import reduce.ast.{untyped => a0}
import reduce.ast.{typed => a1}
import reduce.util.MultiMap


object Types {
  type Errors = Set[Error]
}
import Types._


object Reduce {
  def apply(ast: a0.Ast): (a1.Ast, Errors) = {
    val reduce = new Reduce(ast)
    (reduce.astOut, reduce.errors)
  }
}

case object ReduceM {

  def impure(impureAction: =>Unit) = {impureAction; pure()}

  def pure[A](a: A): ReduceM[A] = ReduceM(a, Set())
  def pure(): ReduceM[Unit] = ReduceM((), Set())

  def raise(errors: Errors): ReduceM[Unit] = ReduceM((), errors)
  def raise(e: Error): ReduceM[Unit] = ReduceM((), Set(e))

  def constrain(a: a1.Exp, b: a1.Exp): ReduceM[Unit] = constrain(a.t, b.t)
  def constrain(a: a1.Type, b: a1.Exp): ReduceM[Unit] = constrain(a, b.t)
  def constrain(a: a1.Type, b: a1.Type): ReduceM[Unit] = when (a != b) { raise(TypeConflict(a, b)) }


  def chooseOverloadExp(overloads: List[ReduceM[a1.Exp]]): ReduceM[a1.Exp] =
    chooseOverload(overloads, a1.InvalidExp)
  def chooseOverloadType(overloads: List[ReduceM[a1.Node]]): ReduceM[a1.Node] =
    chooseOverload(overloads, a1.InvalidExp)
  def chooseOverloadNode(overloads: List[ReduceM[a1.Type]]): ReduceM[a1.Type] =
    chooseOverload(overloads, TError)

  def chooseOverload[A](overloads: List[ReduceM[A]], default: A): ReduceM[A] =
    overloads.foldLeft(List[ReduceM[A]]()) {
      (bestOverloads: List[ReduceM[A]], overload: ReduceM[A]) => bestOverloads match {
        case Nil => List(overload)
        case _ =>
          val errorCount = overload.errors.size
          val bestErrorCount = bestOverloads.head.errors.size
          if (errorCount < bestErrorCount) {
            List(overload)
          } else if (errorCount > bestErrorCount) {
            bestOverloads
          }
          else overload :: bestOverloads
      }
    } match {
      case Nil => pure(default)
      case List(result) => result
      case overloads => raise(AmbiguousOverload(overloads)) >> pure(default)
    }

  def chooseOverload(t: a1.Type, es: List[ReduceM[a1.Exp]]): ReduceM[a1.Exp] =
    chooseOverload(es.map{ e => constrain(t, e.a) >> e }, a1.InvalidExp)

  def when[A](condition: Boolean)(action: ReduceM[Unit]): ReduceM[Unit] =
    if(condition) action else pure()

  def mapM[A, B](as: List[A])(f: A => ReduceM[B]): ReduceM[List[B]] = as match {
    case Nil => pure(Nil)
    case a::rem => f(a) >>= { b => mapM(rem)(f) >>= {bs => pure(b::bs) } }
  }

  def mapM[K, A, B](as: Map[K, A])(f: A => ReduceM[B]): ReduceM[Map[K, B]] = {
    mapM(as.toList){ case (k, a) => for {b <- f(a)} yield (k, b) }.map(_.toMap)
  }

  def mapM[K, A, B](as: MultiMap[K, A])(f: A => ReduceM[B]): ReduceM[MultiMap[K, B]] = {
    mapM(as.map) { as => mapM(as){f}}.map(MultiMap(_))
  }

  def zipWithM[A, B](as: List[A], bs: List[B])(f: (A, B) => ReduceM[Unit]): ReduceM[Unit] = {
    val zip: List[(A, B)] = (as, bs).zipped.map((a, b) => (a, b))
    mapM(zip) { case (a, b) => f(a, b) } >> pure()
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
  def raiseImpure(errs: Errors) = errors = errors union errs

  val history = new Stack[a0.Node]
  def historyContains(n: a0.Node) = history.filter(n.eq(_)).nonEmpty
  def catchCycles[A](input: a0.Node, default: A, mapping: (a0.Node) => A): A =
    if (historyContains(input)) {
      raiseImpure(RecursiveVariableDef(input))
      default
    } else {
      history.push(input)
      val result = mapping(input)
      history.pop()
      result
    }

  val nodes = new IdentityMap[a0.Node, List[ReduceM[a1.Node]]]

  val astOut = astIn.mapValues(mapNamespaceMember(_))
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

  def mapAsType(node: a0.Node): ReduceM[a1.Type] = mapNode(KType, node) match {
    case List(ReduceM(t: a1.Type, errs)) => ReduceM(t, errs)
    case Nil => pure(TError)
    case nodes => nodes.collect{case x@ReduceM(t: a1.Type, _) => x} match {
      case List(ReduceM(t: a1.Type, errs)) => ReduceM(t, errs)
      case Nil => raise(RequiredType(node)) >> pure(TError)
        // throw new Exception(s"$nodes")
      case _ => raise(AmbiguousType(node)) >> pure(TError)
    }
  }

  def mapAsExp(node: a0.Node): List[ReduceM[a1.Exp]] =
    mapNode(KExp(None), node).collect{ case ReduceM(e: a1.Exp, errs) => ReduceM(e, errs) }

  def mapAsExp(ot: Option[a1.Type], node: a0.Node): ReduceM[a1.Exp] = ot match {
    case Some(t) => chooseOverload(t, mapAsExp(node))
    case None => chooseOverload(mapAsExp(node), a1.InvalidExp)
  }

  sealed trait Kind
  case object KAny extends Kind
  case object KType extends Kind
  case class KExp(t: Option[a1.Type]) extends Kind
  // case class KVal(t: a1.Type) extends Kind

  def mapNamespaceMember(_n: a0.Node): a1.Node = {
    val ReduceM(n, errs) = chooseOverload(mapNode(KAny, _n), a1.InvalidExp)
    raiseImpure(errs)
    n
  }
  //   match {
  //   case Nil => a1.InvalidExp
  //   case List(ReduceM(n, errs)) =>
  //     raiseImpure(errs)
  //     n
  //   case units =>
  //     raiseImpure(AmbiguousUnit(units))
  //     a1.InvalidExp
  // }

  def mapNode(kind: Kind, n: a0.Node): List[ReduceM[a1.Node]] = nodes.getOrElseUpdate(n, {
    catchCycles(n, List(raise(RecursiveVariableDef(n)) >> pure(a1.InvalidExp)), (n: a0.Node) => n match {
      case _e: a0.Exp => mapExp(KAny, _e)

      case a0.Namespace(_nodes) =>
        val nodes = _nodes.mapValues(mapNamespaceMember(_))
        pushScope(nodes)
        nodes.map.view.force
        popScope()
        List(pure(a1.Namespace(nodes)))

      case _t: a0.Type => mapType(_t)
    })
  })

  def mapExp(kind: Kind, exp: a0.Exp): List[ReduceM[a1.Node]] = exp match {

    case a0.App(_f, _args) =>
      val overloads = mapAsExp(_f)
      val argOverloads = _args.map{mapAsExp(_)}

      overloads.map {
        (_f: ReduceM[a1.Exp]) => _f.a.t match {
          case a1.TFun(params, ret) =>
            for {
              f <- _f
              args <- mapM((params, argOverloads).zipped.toList) {
                case (param: a1.Type, overloads: List[ReduceM[a1.Exp]]) =>
                  chooseOverload(param, overloads)
              }
            } yield a1.App(f, args) match {

              // Compile time evaluation
              case a1.App(a1.Intrinsic.IAdd, List(VInt(a), VInt(b))) => VInt(a + b)

              case app => app
            }
          case _ => raise(ApplicationOfNonAppliableType(_f.a.t)) >> _f
        }
      }



//       overloads.map(f => for {
//       overloads <- mapAsExp(None, _f)
//       args <- mapM(_args){ mapAsExp(None, _) }
//       app = a1.App(f, args)
//       result <- (f, args) match {

//         // Compile time evaluation
//         case (a1.Intrinsic.IAdd, List(VInt(a), VInt(b))) => pure(VInt(a + b))

//           // Method call syntax
//           // case (a1.Select(e, n), args) =>
//           //   lookupName(n) match {
//           //     case method: Exp => method.t match {
//           //       case TFun(params, args)
//           //   }
//           // }

//         // Typical function application
//         case (f: a1.Exp, args) => f.t match {
//           case a1.TFun(params, ret) =>
//             zipWithM(params, args)((p, a) => constrain(p, a)) >>
//             when(params.length != args.length){ raise(WrongNumArgs(params.length, args.length)) } >>
//               pure(app)
//           case _ => raise(ApplicationOfNonAppliableType(f.t)) >> pure(app)
//         }
//       }
//     }
//     yield result

//       )
//       List(

// )

    case a0.Block(_exps) => List{
      // TODO: reduce to single exp if is single exp
      pushScope()
      for {
        exps <- mapM(_exps){ mapAsExp(None, _) }
      } yield {
        popScope()
        a1.Block(exps)
      }}

    case a0.Cons(_t, _e) =>
      List(for {
        t <- mapAsType(_t)
        e <- mapAsExp(Some(t), _e)
        // _ <- constrain(t, e)
      } yield a1.Cons(t, e))

    case a0.If(_a, _b, _c) =>
      val ReduceM(a, errs) = mapAsExp(Some(TBln), _a)

      a match {
        case v: a1.Val => v match {
          case VBln(true) => mapNode(kind, _b)
          case VBln(false) => mapNode(kind, _c)
          case _ => List(pure(a1.InvalidExp))
        }
        case e => List(for {
          // come up with something more sophisticated if you can
          // I guess I could determine the compatible intersections between overloads of b and c
          // as and bs
          b <- mapAsExp(None, _b)
          c <- mapAsExp(Some(b.t), _c)
          // _ <- constrain(b, c)
        } yield a1.If(a, b, c))
      }

    case a0.Fun(_params, _retType, _body) => List(for {
      params <- mapM(_params) {p => for {t <- mapAsType(p.t)} yield a1.Param(p.n, t)}
      retType <- _retType match {case Some(t) => mapAsType(t); case _ => pure(TError)}
      _ <- impure(pushScope(MultiMap(params.map(p => (p.n, p)): _*)))
      body <- mapAsExp(None, _body)
      _ <- impure(popScope())
    } yield a1.Fun(params, retType, body))

      // push new scope with the params
      // traverse body
      // either enforce the known return type
      // or gather and find supertype of types returned

    case a0.Name(n) => lookupName(n) match {
      case Nil => List(raise(UnknownName(n)) >> pure(a1.Name(n)))
      case exps => exps map { exp => exp match {
        case n: a1.Namespace => pure(n)
        case i: a1.Intrinsic => pure(i)
        case v: a1.Val => pure(v)
        case t: a1.Type => pure(t)
        case e: a1.Exp => pure(a1.Name(n, e))
        }
      }
    }

    case a0.Select(_e, memberName) =>
      val exps = mapNode(KAny, _e)
      exps flatMap { case ReduceM(e, errs) => { e match {

        // TODO: Filter to the required kind
        case a1.Namespace(units) => units.get(memberName).map(pure(_))

        // TODO: Filter to the required kind
        case a1.VObj(typ, members) => members.get(memberName).map(pure(_))
        // match {
        //   case Nil => raise(NonExistentMember(memberName)) >> pure(List(a1.InvalidExp))
        //   case xs => pure(xs)
        // }

        // TODO: Filter to the required kind
        case e: a1.Exp => e.t match {
          case a1.Struct(_, members) => members.get(memberName).map(pure(_))
          // match {
          //   case _::_::_ => ???
          //   case Nil => raise(NonExistentMember(memberName)) >> pure(List(a1.InvalidExp))
          //   case t::Nil => pure(List(a1.Select(e, memberName)))
          // }
          case TError => throw new Exception(s"$e")
        }
      } }.map(raise(errs) >> _) }

    case a0.Var(n, _e) =>
      List(for {
        e <- mapAsExp(None, _e)
      } yield {
        addLocalBinding(n, e)
        a1.Var(n, e)
      })

    case v: a0.Val => List(mapVal(v))
  }

  def mapVal(v: a0.Val): ReduceM[a1.Val] = v match {
    case a0.VObj(_t, _members) => for {
      t <- mapAsType(_t)
      members <- mapM(_members){mapVal}
    } yield a1.VObj(t, members)
    case v: ValAtom => pure(v)
  }

  def mapType(t: a0.Type): List[ReduceM[a1.Type]] = List(t match {
    case t: TypeAtom => pure(t)
    case a0.TFun(_params, _ret) => for {
      params <- mapM(_params){mapAsType}
      ret <- mapAsType(_ret)
    } yield a1.TFun(params, ret)
    case a0.Struct(name, _fields) => for {
      fields <- mapM(_fields) {mapAsType}
    } yield a1.Struct(name, fields)
  })

  astOut.map.view.force
}

