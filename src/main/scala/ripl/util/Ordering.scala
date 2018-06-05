package ripl.util

sealed trait Ordering
case object Ordering {
  def apply(a: Int, b: Int): Ordering = {
    if (a < b) Ordering.LT
    else if (a > b) Ordering.GT
    else Ordering.EQ
  }

  implicit object semigroup extends cats.Semigroup[Ordering] {
    def combine(x: Ordering, y: Ordering): Ordering = x match {
      case EQ => y
      case _  => x
    }
  }
  case object LT extends Ordering
  case object GT extends Ordering
  case object EQ extends Ordering
}
import Ordering.semigroup
