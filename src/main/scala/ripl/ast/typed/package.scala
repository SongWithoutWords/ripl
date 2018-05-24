package ripl.ast

import ripl.ast.typed._
import ripl.util.MultiMap

package object typed {
  type Nodes = MultiMap[String, Node]
  type Ast = Nodes
}
