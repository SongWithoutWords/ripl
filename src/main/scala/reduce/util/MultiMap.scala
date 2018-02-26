package reduce.util

import scala.collection.immutable.{Map, Set}

object MultiMap
{
  def apply[A, B](pairs: (A, B)*): MultiMap[A, B] = {
    pairs.foldLeft(MultiMap(Map[A, Set[B]]())) { (map, pair) =>
      map.add(pair._1, pair._2)
    }
  }
}

case class MultiMap[A, B](map: Map[A, Set[B]]) {

  def add(a: A, b: B) = MultiMap(map.updated(a, get(a) + b))
  def apply(a: A): Set[B] = map.get(a) match {
    case Some(set) => set
    case None => Set()
  }
  def get(a: A) = apply(a)

  def mapValues[C](f: B => C): MultiMap[A, C] =
    MultiMap(map.mapValues{ (s: Set[B]) => s.map(f)})
}

