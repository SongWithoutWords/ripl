package ripl.ast

import ripl.ast.untyped._
import ripl.util.MultiMap

package object untyped {
  type Definitions = MultiMap[String, Exp]
}
