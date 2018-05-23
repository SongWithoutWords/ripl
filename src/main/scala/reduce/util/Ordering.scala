package reduce.util

case object Ordering {
  def apply(a: Int, b: Int): Ordering = {
    if (a < b) Ordering.LT
    else if (a > b) Ordering.GT
    else Ordering.EQ
  }

  case object LT extends Ordering {
    def <>(rhs: Ordering) = LT
  }
  case object GT extends Ordering {
    def <>(rhs: Ordering) = GT
  }
  case object EQ extends Ordering {
    def <>(rhs: Ordering) = rhs
  }
}

sealed trait Ordering {
  def <>(rhs: Ordering): Ordering
}

