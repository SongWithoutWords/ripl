package ripl.ast.common

import enumeratum._

import ripl.ast.typed.Type

sealed trait TypeAtom extends Type with EnumEntry { val n: String }

case object TypeAtom extends Enum[TypeAtom] {
  val values = findValues

  case object TBln extends TypeAtom { val n = "Bln" }
  case object TFlt extends TypeAtom { val n = "Flt" }
  case object TInt extends TypeAtom { val n = "Int" }
  case object TNone extends TypeAtom { val n = "None" }
  case object TStr extends TypeAtom { val n = "Str" }
}
