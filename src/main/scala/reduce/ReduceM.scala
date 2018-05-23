package reduce

import reduce.ast.{untyped => a0}
import reduce.ast.{typed => a1}

import reduce.util.MultiMap
import reduce.util.Ordering


object Types {
  type Errors = Set[Error]
}

import Types._

case object ReduceInfo {
  def apply(): ReduceInfo = ReduceInfo(Set(), 0)
}

case class ReduceInfo(errors: Set[Error], implicitConversionCount: Int) {
  def <>(rhs: ReduceInfo) = ReduceInfo(
    errors union rhs.errors,
    implicitConversionCount + rhs.implicitConversionCount)

  def compare(rhs: ReduceInfo): Ordering =
    Ordering(errors.size, rhs.errors.size) <>
    Ordering(implicitConversionCount, rhs.implicitConversionCount)
}

case class ReduceM[+A](value: A, info: ReduceInfo) {

  def map[B](f: A => B) = ReduceM(f(value), info)

  def >>[B](rhs: ReduceM[B]) = ReduceM(rhs.value, info <> rhs.info)

  def >>=[B](f: A => ReduceM[B]): ReduceM[B] = {
    val rhs = f(value)
    ReduceM(rhs.value, info <> rhs.info)
  }

  def flatMap[B](f: A => ReduceM[B]): ReduceM[B] = >>=(f)
}

case object ReduceM {

  def impure(impureAction: =>Unit) = {impureAction; pure()}

  def pure[A](a: A): ReduceM[A] = ReduceM(a, ReduceInfo())
  def pure(): ReduceM[Unit] = ReduceM((), ReduceInfo())

  def raise(info: ReduceInfo): ReduceM[Unit] = ReduceM((), info)
  def raise(errors: Errors): ReduceM[Unit] = raise(ReduceInfo(errors, 0))
  def raise(e: Error): ReduceM[Unit] = raise(Set(e))

  def constrain(a: a1.Exp, b: a1.Exp): ReduceM[Unit] = constrain(a.t, b.t)
  def constrain(a: a1.Type, b: a1.Exp): ReduceM[Unit] = constrain(a, b.t)
  def constrain(a: a1.Type, b: a1.Type): ReduceM[Unit] = when (a != b) { raise(TypeConflict(a, b)) }

  def chooseOverload[A](overloads: List[ReduceM[A]], default: A): ReduceM[A] =
    overloads.foldLeft[List[ReduceM[A]]](Nil) {
      (bestOverloads, overload) => bestOverloads match {
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
      case Nil => pure(default)
      case List(result) => result
      case overloads => raise(AmbiguousOverload(overloads)) >> pure(default)
    }

  def chooseOverload(t: a1.Type, es: List[ReduceM[a1.Exp]]): ReduceM[a1.Exp] =
    chooseOverload(es.map{ e => constrain(t, e.value) >> e }, a1.InvalidExp)

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

