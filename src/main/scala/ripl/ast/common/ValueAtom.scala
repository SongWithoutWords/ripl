package ripl.ast.common

import ripl.ast.common._
import ripl.ast.common.TypeAtom._
import ripl.ast.{untyped => a0, typed => a1}

object ImplicitConversions {
  implicit def ImplicitBln(b: Boolean): VBln = VBln(b)
  implicit def ImplicitFlt(f: Float): VFlt = VFlt(f)
  implicit def ImplicitInt(i: Int): VInt = VInt(i)
}

sealed trait ValAtom extends a0.Val with a1.Val

case class VBln(b: Boolean) extends ValAtom {
  def t = TBln
}

case class VFlt(f: Float) extends ValAtom {
  def t = TFlt
}

case class VInt(i: Int) extends ValAtom {
  def t = TInt
}

case class VStr(i: String) extends ValAtom {
  def t = TStr
}
