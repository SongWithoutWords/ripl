package ripl.ast.common

import ripl.ast.typed.Type


sealed trait TypeAtom extends Type
case object TBln extends TypeAtom
case object TError extends TypeAtom
case object TFlt extends TypeAtom
case object TInt extends TypeAtom
case object TNone extends TypeAtom
case object TStr extends TypeAtom

