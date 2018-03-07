package reduce.ast

import reduce.ast.typed._
import reduce.util.MultiMap

package object typed {
  type Nodes = MultiMap[String, Node]
  type Ast = Nodes
}
