package reduce.ast.common

import reduce.ast.{untyped => a0, typed => a1}
import reduce.ast.common._

object ImplicitConversions {
  implicit def ImplicitBln(b: Boolean): VBln = VBln(b)
  implicit def ImplicitInt(b: Int): VInt = VInt(b)
}

sealed trait ValAtom extends a0.Val with a1.Val

case class VBln(b: Boolean) extends ValAtom {
  def t = TBln
}

case class VInt(i: Int) extends ValAtom {
  def t = TInt
}



