// Module to allow importing 'Visibility' distinctly qualified.
package ripl.llvm.pure.ast

import LLVM.Prelude

// <http://llvm.org/docs/LangRef.html#visibility>

sealed trait Visibility
case object Visibility {
  case object Default extends Visibility
  case object Hidden extends Visibility
  case object Protected extends Visibility
}
