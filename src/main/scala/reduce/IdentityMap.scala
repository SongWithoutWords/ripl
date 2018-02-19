package reduce

import scala.collection.mutable.HashMap

class IdentityMap[A <: AnyRef, B] extends HashMap[A, B] {
  protected override def elemEquals(k1: A, k2: A): Boolean = k1 eq k2
  protected override def elemHashCode(k: A): Int = System.identityHashCode(k)
}

