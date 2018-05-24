package reduce

import ast.typed._
import ast.common._
import util.MultiMap

object BuiltInConversions {
  val entries: MultiMap[Type, Exp] = MultiMap(
    TInt -> Intrinsic.ItoF
  )
}


