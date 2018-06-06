package ripl.reduce

import scala.collection.immutable.Set
import scala.collection.mutable.Stack

import cats._
import cats.data._
import cats.implicits._

import ripl.ast.common._
import ripl.ast.common.TypeAtom._
import ripl.ast.{untyped => a0}
import ripl.ast.{typed => a1}
import a1.{TError}
import ripl.util.IdentityMap
import ripl.util.MultiMap
import ripl.util.Ordering

import Types._
import ReduceM._

object Reduce {
  def apply(ast: a0.Ast): (a1.Ast, Errors) = {
    val reduce = new Reduce(ast)
    (reduce.astOut, reduce.errors)
  }
}

class Reduce(val astIn: a0.Ast) {
  var errors = Set[Error]()
  def raiseImpure(e: Error) = errors = errors + e
  def raiseImpure(errs: Errors) = errors = errors union errs

  def constrain(requiredType: a1.Type, exp: ReduceM[a1.Exp]): ReduceM[a1.Exp] =
    exp >>= { constrain(requiredType, _) }

  def constrain(requiredType: a1.Type, exp: a1.Exp): ReduceM[a1.Exp] = {

    val expType = exp.t

    if (expType == requiredType) pure(exp)
    else if (expType == TError || requiredType == TError) pure(exp)
    else {
      lookupImplicitConversions(expType).collect { conversion =>
        conversion.t match {
          case a1.TFun(List(tIn), tOut)
              if tIn == expType && tOut == requiredType =>
            conversion
        }
      } match {
        case Nil => raise(TypeConflict(requiredType, expType)) >> pure(exp)
        case List(conversion) =>
          raiseImplicitConversion() >> pure(evalApp(a1.App(conversion, exp)))
        case conversions =>
          raise(AmbiguousImplicitConversions(conversions)) >> pure(exp)
      }
    }
  }

  def chooseOverload[A](overloads: List[ReduceM[A]], default: A): ReduceM[A] =
    overloads.foldLeft[List[ReduceM[A]]](Nil) { (bestOverloads, overload) =>
      bestOverloads match {
        case Nil => List(overload)
        case _ =>
          val bestOverloadInfo = bestOverloads.head.info
          overload.info.compare(bestOverloads.head.info) match {
            case Ordering.LT => List(overload)
            case Ordering.GT => bestOverloads
            case Ordering.EQ => overload :: bestOverloads
          }
      }
    } match {
      case Nil          => pure(default)
      case List(result) => result
      case overloads    => raise(AmbiguousOverload(overloads)) >> pure(default)
    }

  def chooseOverload(t: a1.Type, es: List[ReduceM[a1.Exp]]): ReduceM[a1.Exp] =
    chooseOverload(es.map { constrain(t, _) }, a1.InvalidExp)

  def findCycle(
      node: a0.Node,
      history: List[a1.Cycle.Component]
  ): List[a1.Cycle.Component] = {
    def impl(
        accum: List[a1.Cycle.Component],
        rem: List[a1.Cycle.Component]
    ): List[a1.Cycle.Component] = rem match {
      case Nil => Nil
      case head :: tail =>
        if (head.node eq node) (head :: accum).reverse
        else impl(head :: accum, tail)
    }
    impl(Nil, history)
  }

  var history = List[a1.Cycle.Component]()

  def addToHistory(
      input: a0.Node,
      mapping: a0.Node => List[ReduceM[a1.Node]]
  ): List[ReduceM[a1.Node]] = {
    val cycleComponent = input match {
      case f @ a0.Fun(params, retTypeOpt, _) =>
        a1.Cycle.Fun(
          f,
          params.traverse { case a0.Param(_, t) => mapAsType(t) }.value,
          retTypeOpt.traverse { mapAsType }.value
        )
      case _ => a1.Cycle.Node(input)
    }
    history = cycleComponent :: history
    val result = mapping(input)
    history = history.tail
    result
  }

  def catchCycles(
      input: a0.Node,
      mapping: a0.Node => List[ReduceM[a1.Node]]
  ): List[ReduceM[a1.Node]] = findCycle(input, history) match {
    case Nil => mapping(input)
    case cycle =>
      List(
        when(cycle.collect { case f: a1.Cycle.Fun => f } != cycle) {
          raise(RecursiveVariableDef(a1.Cycle(cycle)))
        } >> (cycle match {
          case a1.Cycle.Fun(_, paramTypes, optionalRetType) :: _ =>
            optionalRetType match {
              case Some(_) => pure()
              case None =>
                raise(RecursiveFunctionLacksExplicitReturnType(a1.Cycle(cycle)))
            }
          case _ => pure()
        }) >> pure(a1.Cycle(cycle))
      )
  }

  def catchCyclesAndAddToHistory(
      input: a0.Node,
      mapping: a0.Node => List[ReduceM[a1.Node]]
  ): List[ReduceM[a1.Node]] = catchCycles(input, addToHistory(_, mapping))

  val nodes = new IdentityMap[a0.Node, List[ReduceM[a1.Node]]]

  val astOut = astIn.mapValues(mapNamespaceMember(_))
  val intrinsics = MultiMap(a1.Intrinsic.values.map(i => (i.n, i)): _*)
  val builtInTypes = MultiMap(TypeAtom.values.map(t => (t.n, t)): _*)

  type Scope = MultiMap[String, a1.Node]
  var scopes: List[Scope] = Nil

  def pushScope() = scopes = MultiMap[String, a1.Node]() :: scopes
  def pushScope(s: Scope) = scopes = s :: scopes
  def popScope() = scopes = scopes.tail

  def addLocalBinding(n: String, v: a1.Node) =
    scopes = scopes.head.add(n, v) :: scopes.tail

  def lookupName(n: String): List[a1.Node] =
    astOut.get(n).toList ++
      intrinsics.get(n) ++
      builtInTypes.get(n) ++
      scopes.flatMap(_.get(n))

  def lookupImplicitConversions(t: a1.Type): List[a1.Exp] =
    BuiltInConversions.entries(t)

  def mapAsType(node: a0.Node): ReduceM[a1.Type] = mapNode(KType, node) match {
    case List(ReduceM(t: a1.Type, info)) => ReduceM(t, info)
    case Nil                             => pure(TError)
    case nodes =>
      nodes.collect { case x @ ReduceM(t: a1.Type, _) => x } match {
        case List(ReduceM(t: a1.Type, info)) => ReduceM(t, info)
        case Nil                             => raise(RequiredType(node)) >> pure(TError)
        // throw new Exception(s"$nodes")
        case _ => raise(AmbiguousType(node)) >> pure(TError)
      }
  }

  def mapAsExp(node: a0.Node): List[ReduceM[a1.Exp]] =
    mapNode(KExp(None), node).collect {
      case ReduceM(e: a1.Exp, errs) => ReduceM(e, errs)
    }

  def mapAsExp(ot: Option[a1.Type], node: a0.Node): ReduceM[a1.Exp] = ot match {
    case Some(t) => chooseOverload(t, mapAsExp(node))
    case None    => chooseOverload(mapAsExp(node), a1.InvalidExp)
  }

  sealed trait Kind
  case object KAny extends Kind
  case object KType extends Kind
  case class KExp(t: Option[a1.Type]) extends Kind
  // case class KVal(t: a1.Type) extends Kind

  def mapNamespaceMember(_n: a0.Node): a1.Node = {
    val ReduceM(n, info) =
      chooseOverload(mapNodeAndAddToHistory(KAny, _n), a1.InvalidExp)
    raiseImpure(info.errors)
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

  def mapNodeWithoutCycleDetection(n: a0.Node) = n match {
    case _t: a0.Type => mapType(_t)

    case _e: a0.Exp => mapExp(KAny, _e)

    case a0.Namespace(_nodes) =>
      val nodes = _nodes.mapValues(mapNamespaceMember(_))
      pushScope(nodes)
      nodes.underlyingMap.view.force
      popScope()
      List(pure(a1.Namespace(nodes)))
  }

  def mapNode(kind: Kind, n: a0.Node): List[ReduceM[a1.Node]] =
    nodes.getOrElseUpdate(n, catchCycles(n, mapNodeWithoutCycleDetection))

  def mapNodeAndAddToHistory(kind: Kind, n: a0.Node): List[ReduceM[a1.Node]] =
    nodes.getOrElseUpdate(
      n,
      catchCyclesAndAddToHistory(n, mapNodeWithoutCycleDetection)
    )

  def evalApp(app: a1.App): a1.Exp = app match {

    case a1.App(a1.Intrinsic.IAdd, List(VInt(a), VInt(b))) => VInt(a + b)
    case a1.App(a1.Intrinsic.FAdd, List(VFlt(a), VFlt(b))) => VFlt(a + b)
    case a1.App(a1.Intrinsic.ItoF, List(VInt(a)))          => VFlt(a)
    case a1.App(a1.Intrinsic.FtoI, List(VFlt(a)))          => VInt(a.toInt)

    case app => app
  }

  def mapExp(kind: Kind, exp: a0.Exp): List[ReduceM[a1.Node]] = exp match {

    case a0.App(_f, _args) =>
      def chooseArgs(
          _f: ReduceM[a1.Exp],
          argOverloads: List[List[ReduceM[a1.Exp]]]
      ): ReduceM[a1.Exp] = {
        _f.value.t match {
          case a1.TFun(params, ret) =>
            val paramCount = params.length
            val argCount = argOverloads.length
            when(paramCount != argCount) {
              raise(WrongNumArgs(paramCount, argCount))
            } >> {
              for {
                f <- _f
                args <- (params, argOverloads).zipped.toList.traverse {
                  case (param: a1.Type, overloads: List[ReduceM[a1.Exp]]) =>
                    chooseOverload(param, overloads)
                }
              } yield evalApp(a1.App(f, args))
            }
          case TError => _f
          case _      => raise(ApplicationOfNonAppliableType(_f.value.t)) >> _f
        }
      }

      val overloads = mapAsExp(_f)
      val argOverloads = _args.map { mapAsExp(_) }
      overloads.map(chooseArgs(_, argOverloads)) ++ (_f match {

        // Method call syntax
        case a0.Select(_e, name) =>
          val methodArgs = mapAsExp(_e) :: argOverloads
          val methodOverloads = lookupName(name)
            .collect { case e: a1.Exp => e }
            .map { e =>
              when(e == a1.InvalidExp) { raise(UseOfInvalidExp) } >> pure(e)
            }
          methodOverloads.map(chooseArgs(_, methodArgs))
        case _ => Nil
      }) ++ {

        // Application syntax
        val appArgs = overloads :: argOverloads
        val appOverloads = lookupName("apply")
          .collect {
            case e: a1.Exp => {
              e match {
                case a1.InvalidExp => raise(UseOfInvalidExp)
                // case a1.RecursiveDef(cycle) => // TODO: Not sure if this is necessary or not
                //   raise (RecursiveVariableDef(cycle))
                case _ => pure()
              }
            } >> pure(e)
          }
        appOverloads.map(chooseArgs(_, appArgs))
      }

    case a0.Block(_exps) =>
      List {
        // TODO: reduce to single exp if is single exp
        pushScope()
        for {
          exps <- _exps.traverse { mapAsExp(None, _) }
        } yield {
          popScope()
          a1.Block(exps)
        }
      }

    case a0.Cons(_t, _e) =>
      List(for {
        t <- mapAsType(_t)
        e <- mapAsExp(Some(t), _e)
        // _ <- constrain(t, e)
      } yield a1.Cons(t, e))

    case a0.If(_a, _b, _c) =>
      val ReduceM(a, info) = mapAsExp(Some(TBln), _a)

      {
        a match {
          case v: a1.Val =>
            v match {
              case VBln(true)  => mapNode(kind, _b)
              case VBln(false) => mapNode(kind, _c)
              case _           => List(pure(a1.InvalidExp))
            }
          case e =>
            List(for {
              // come up with something more sophisticated if you can
              // I guess I could determine the compatible intersections between overloads of b and c
              // as and bs
              b <- mapAsExp(None, _b)
              c <- mapAsExp(Some(b.t), _c)
              // _ <- constrain(b, c)
            } yield a1.If(a, b, c))
        }
      }.map(raise(info) >> _)

    case a0.Fun(_params, _retType, _body) =>
      List(for {
        params <- _params.traverse { p =>
          for { t <- mapAsType(p.t) } yield a1.Param(p.n, t)
        }

        retType <- _retType.traverse { mapAsType(_) }
        _ <- impure(pushScope(MultiMap(params.map(p => (p.n, p)): _*)))
        body <- mapAsExp(retType, _body)
        _ <- impure(popScope())
      } yield a1.Fun(params, retType.getOrElse(body.t), body))

    // push new scope with the params
    // traverse body
    // either enforce the known return type
    // or gather and find supertype of types returned

    case a0.Name(n) =>
      lookupName(n) match {
        case Nil => List(raise(UnknownName(n)) >> pure(a1.Name(n)))
        case exps =>
          exps map { exp =>
            exp match {
              case n: a1.Namespace => pure(n)
              case i: a1.Intrinsic => pure(i)
              case v: a1.Val       => pure(v)
              case t: a1.Type      => pure(t)
              case e: a1.Exp       => pure(a1.Name(n, e))
            }
          }
      }

    case a0.Select(_e, memberName) =>
      val exps = mapNode(KAny, _e)
      exps flatMap {
        case ReduceM(e, info) => {
          e match {

            // TODO: Filter to the required kind
            case a1.Namespace(units) => units.get(memberName).map(pure(_))

            // TODO: Filter to the required kind
            case a1.VObj(typ, members) => members.get(memberName).map(pure(_))
            // match {
            //   case Nil => raise(NonExistentMember(memberName)) >> pure(List(a1.InvalidExp))
            //   case xs => pure(xs)
            // }

            // TODO: Filter to the required kind
            case e: a1.Exp =>
              e.t match {
                case a1.Struct(_, memberTypes) =>
                  memberTypes.get(memberName).map { t =>
                    pure(a1.Select(e, memberName, t))
                  }
                case t =>
                  List(
                    raise(SelectionFromNonStructType(t)) >> pure(a1.InvalidExp)
                  )
              }
          }
        }.map(raise(info) >> _)
      }

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
    case a0.VObj(_t, _members) =>
      for {
        t <- mapAsType(_t)
        // members <- _members.traverse { mapVal(_) }
        members <- MultiMap.instances[String]().traverse(_members)(mapVal)
      } yield a1.VObj(t, members)
    case v: ValAtom => pure(v)
  }

  def mapType(t: a0.Type): List[ReduceM[a1.Type]] =
    List(t match {
      case t: TypeAtom => pure(t)
      case a0.TFun(_params, _ret) =>
        for {
          params <- _params.traverse { (n: a0.Node) =>
            mapAsType(n)
          }
          ret <- mapAsType(_ret)
        } yield a1.TFun(params, ret)
      case a0.Struct(name, _fields) =>
        for {
          fields <- MultiMap.instances().traverse(_fields)(mapAsType)
        } yield a1.Struct(name, fields)
    })

  astOut.underlyingMap.view.force
}
