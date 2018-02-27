package reduce.util

import scala.collection.immutable.Map

object MultiMap
{
  def apply[A, B](pairs: (A, B)*): MultiMap[A, B] = {
    pairs.foldLeft(MultiMap(Map[A, List[B]]())) { (map, pair) =>
      map.add(pair._1, pair._2)
    }
  }
}

case class MultiMap[A, +B](map: Map[A, List[B]]) {

  def add[B1 >: B](a: A, b: B1): MultiMap[A, B1] = MultiMap(map.updated(a, b :: get(a)))
  def apply(a: A): List[B] = map.get(a) match {
    case Some(l) => l
    case None => Nil
  }
  def get(a: A) = apply(a)

  def mapValues[C](f: B => C): MultiMap[A, C] =
    MultiMap(map.mapValues{ _.map(f) }) // (s: Set[B]) => s.map(f)})
}

