package ripl.reduce

import ripl.ast.typed._
import ripl.ast.common._
import ripl.ast.common.TypeAtom._
import ripl.util.MultiMap

object BuiltInConversions {
  val entries: MultiMap[Type, Exp] = MultiMap(
    TInt -> Intrinsic.ItoF
  )
}


