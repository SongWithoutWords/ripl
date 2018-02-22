package reduce.util

import scala.collection.immutable.Map

case class MultiMap[A, B](map: Map[A, Set[B]]) {
  def apply(a: A) = map(a)

  def mapValues[C](f: B => C): MultiMap[A, C] =
    MultiMap(map.mapValues{ (s: Set[B]) => s.map(f)})
}

