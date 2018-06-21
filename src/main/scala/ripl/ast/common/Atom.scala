package ripl.ast.common

import ripl.ast.common._
import ripl.ast.common.TypeAtom._
import ripl.ast.{untyped => a0, typed => a1}

import ripl.parse.recursive.Token

object ImplicitConversions {
  implicit def ImplicitBln(b: Boolean): VBln = VBln(b)
  implicit def ImplicitFlt(f: Float): VFlt   = VFlt(f)
  implicit def ImplicitInt(i: Int): VInt     = VInt(i)
}

sealed trait Atom extends Token with a0.Exp

// `Name` chosen over `Symbol` because scala defines symbol
case class Name(s: String) extends Atom

sealed trait ValAtom extends Atom with a0.Val with a1.Val

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
