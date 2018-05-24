package ripl.ast

import ripl.ast.untyped._
import ripl.util.MultiMap

package object untyped {
  type Ast = Nodes
  // type Units = MultiMap[String, Node]
  type Nodes = MultiMap[String, Node]
}
