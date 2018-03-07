package reduce.ast

import reduce.ast.untyped._
import reduce.util.MultiMap

package object untyped {
  type Ast = Nodes
  // type Units = MultiMap[String, Node]
  type Nodes = MultiMap[String, Node]
}
