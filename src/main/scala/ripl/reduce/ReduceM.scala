package ripl.reduce

import ripl.ast.{untyped => a0}
import ripl.ast.{typed => a1}

import ripl.util.MultiMap
import ripl.util.Ordering


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

  def raiseImplicitConversion(): ReduceM[Unit] = raise(ReduceInfo(Set(), 1))

  def when[A](condition: Boolean)(action: ReduceM[Unit]): ReduceM[Unit] =
    if(condition) action else pure()

  def mapM[A, B](ma: Option[A])(f: A => ReduceM[B]): ReduceM[Option[B]] = ma match {
    case None => pure(None)
    case Some(a) => f(a).map(Some(_))
  }

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

