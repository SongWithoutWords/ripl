// Module to allow importing 'COMDAT.SelectionKind' distinctly qualified.
package ripl.llvm.pure.ast

// <http://llvm.org/docs/LangRef.html#comdats>

sealed trait SelectionKind
case object SelectionKind {
  case object Any extends SelectionKind
  case object ExactMatch extends SelectionKind
  case object Largest extends SelectionKind
  case object NoDuplicates extends SelectionKind
  case object SameSize extends SelectionKind
}
