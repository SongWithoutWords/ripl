// Module to allow importing 'DLL.StorageClass' distinctly qualified.
package ripl.llvm.ast

// <http://llvm.org/docs/LangRef.html#dll-storage-classes>
sealed trait StorageClass
case object StorageClass {
  case object Import extends StorageClass
  case object Export extends StorageClass
}
