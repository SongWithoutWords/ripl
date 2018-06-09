// Module to allow importing 'COMDAT.SelectionKind' distinctly qualified.
package ripl.llvm.pure.ast

// <http://llvm.org/docs/LangRef.html#comdats>

sealed trait SelectionKind
case object SelectionKind {
  case object Any
  case object ExactMatch
  case object Largest
  case object NoDuplicates
  case object SameSize
}
