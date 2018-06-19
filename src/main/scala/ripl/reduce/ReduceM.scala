package ripl.reduce

import cats._
import cats.data._
import cats.implicits._

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

  implicit object semigroup extends Semigroup[ReduceInfo] {
    def combine(x: ReduceInfo, y: ReduceInfo): ReduceInfo =
      ReduceInfo(
        x.errors union y.errors,
        x.implicitConversionCount + y.implicitConversionCount
      )
  }
}

case class ReduceInfo(errors: Set[Error], implicitConversionCount: Int) {
  def compare(rhs: ReduceInfo): Ordering =
    Ordering(errors.size, rhs.errors.size) |+|
      Ordering(implicitConversionCount, rhs.implicitConversionCount)
}

case class ReduceM[+A](value: A, info: ReduceInfo)

case object ReduceM {

  implicit object instances extends Monad[ReduceM] {

    def pure[A](a: A): ReduceM[A] = ReduceM(a, ReduceInfo())
    def pure(): ReduceM[Unit]     = pure(())

    def flatMap[A, B](ma: ReduceM[A])(f: A => ReduceM[B]): ReduceM[B] = {
      val mb = f(ma.value)
      ReduceM(mb.value, ma.info |+| mb.info)
    }

    def tailRecM[A, B](a: A)(f: A => ReduceM[Either[A, B]]): ReduceM[B] =
      f(a) match {
        case ReduceM(Left(nextA), info) =>
          val ReduceM(nextValue, nextInfo) = tailRecM(nextA)(f)
          ReduceM(nextValue, info |+| nextInfo)

        case ReduceM(Right(b), info) => ReduceM(b, info)
      }
  }

  def impure(impureAction: => Unit) = { impureAction; pure() }

  def pure[A](a: A): ReduceM[A] = instances.pure(a)
  def pure(): ReduceM[Unit]     = instances.pure()

  def raise(info: ReduceInfo): ReduceM[Unit] = ReduceM((), info)
  def raise(errors: Errors): ReduceM[Unit]   = raise(ReduceInfo(errors, 0))
  def raise(e: Error): ReduceM[Unit]         = raise(Set(e))

  def raiseImplicitConversion(): ReduceM[Unit] = raise(ReduceInfo(Set(), 1))

  def when(condition: Boolean)(action: ReduceM[Unit]): ReduceM[Unit] =
    if (condition) action else pure()
}
