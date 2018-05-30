package ripl.ast.common

import ripl.ast.{untyped => a0, typed => a1}


sealed trait TypeAtom extends a0.Type with a1.Type
case object TBln extends TypeAtom
case object TError extends TypeAtom
case object TFlt extends TypeAtom
case object TInt extends TypeAtom
case object TNone extends TypeAtom
case object TStr extends TypeAtom

