// Module to allow importing 'ThreadLocalStorage.Model' distinctly qualified.
package ripl.llvm.pure.ast

// <http://llvm.org/docs/LangRef.html#thread-local-storage-models>
sealed trait ThreadLocalStorageModel
case object ThreadLocalStorageModel {
    case object GeneralDynamic extends ThreadLocalStorageModel
    case object LocalDynamic extends ThreadLocalStorageModel
    case object InitialExec extends ThreadLocalStorageModel
    case object LocalExec extends ThreadLocalStorageModel
}


