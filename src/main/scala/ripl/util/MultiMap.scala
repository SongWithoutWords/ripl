package ripl.util

import scala.collection.immutable.Map

import cats._
import cats.data._
import cats.implicits._

object MultiMap {

  def apply[A, B](pairs: (A, B)*): MultiMap[A, B] = {
    pairs.foldLeft(MultiMap(Map[A, List[B]]())) { (map, pair) =>
      map.add(pair._1, pair._2)
    }
  }

  implicit def instances[K](): Traverse[({ type L[A] = MultiMap[K, A] })#L] =
    new Traverse[({ type L[A] = MultiMap[K, A] })#L] // partial application of multimap type
    {

      // for functor
      override def map[A, B](
          multiMap: MultiMap[K, A]
        )(f: A => B
        ): MultiMap[K, B] =
        MultiMap(multiMap.underlyingMap.mapValues(_.map(f)))

      // for foldable
      def foldLeft[A, B](multiMap: MultiMap[K, A], b0: B)(f: (B, A) => B): B = {
        multiMap.underlyingMap.foldLeft(b0) { (b1: B, kl: (K, List[A])) =>
          kl._2.foldLeft(b1)(f)
        }
      }

      def foldRight[A, B](
          fa: MultiMap[K, A],
          lb: cats.Eval[B]
        )(f: (A, cats.Eval[B]) => cats.Eval[B]
        ): cats.Eval[B] = ???

      // for traversable
      def traverse[G[_], A, B](
          multiMap: MultiMap[K, A]
        )(f: A => G[B]
        )(implicit evidence$1: Applicative[G]
        ): G[MultiMap[K, B]] = {
        multiMap.underlyingMap.toList
          .traverse { kv: (K, List[A]) =>
            kv._2.traverse(f).map((kv._1, _))
          }
          .map((l: List[(K, List[B])]) => MultiMap(l.toMap))
      }
    }
}

case class MultiMap[A, +B](underlyingMap: Map[A, List[B]]) {

  def add[B1 >: B](a: A, b: B1): MultiMap[A, B1] =
    MultiMap(underlyingMap.updated(a, b :: get(a)))

  def apply(a: A): List[B] = underlyingMap.get(a) match {
    case Some(l) => l
    case None    => Nil
  }
  def get(a: A) = apply(a)

  def mapValues[C](f: B => C): MultiMap[A, C] =
    MultiMap(underlyingMap.mapValues { _.map(f) })

  def toList: List[(A, B)] = {
    underlyingMap.toList.flatMap { case (a, bs) => bs.map((a, _)) }
  }
}
