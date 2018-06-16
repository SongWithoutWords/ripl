// Module to allow importing 'Linkage' distinctly qualified.
package ripl.llvm.ast

// <http://llvm.org/docs/LangRef.html#linkage>

sealed trait Linkage
case object Linkage {
  case object Private extends Linkage
  case object Internal extends Linkage
  case object AvailableExternally extends Linkage
  case object LinkOnce extends Linkage
  case object Weak extends Linkage
  case object Common extends Linkage
  case object Appending extends Linkage
  case object ExternWeak extends Linkage
  case object LinkOnceODR extends Linkage
  case object WeakODR extends Linkage
  case object External extends Linkage
}
