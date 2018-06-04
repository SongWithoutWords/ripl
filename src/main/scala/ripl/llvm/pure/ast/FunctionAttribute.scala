// Module to allow importing 'FunctionAttribute' distinctly qualified.
package ripl.llvm.pure.ast

// <http://llvm.org/docs/LangRef.html#function-attributes>
sealed trait FunctionAttribute
case object FunctionAttribute {
  case object NoReturn
  case object NoUnwind
  case object ReadNone
  case object ReadOnly
  case object NoInline
  case object NoRecurse
  case object AlwaysInline
  case object MinimizeSize
  case object OptimizeForSize
  case object OptimizeNone
  case object StackProtect
  case object StackProtectReq
  case object StackProtectStrong
  case object NoRedZone
  case object NoImplicitFloat
  case object Naked
  case object InlineHint
  case class StackAlignment(n: Int)
  case object ReturnsTwice
  case object UWTable
  case object NonLazyBind
  case object Builtin
  case object NoBuiltin
  case object Cold
  case object JumpTable
  case object NoDuplicate
  case object SanitizeAddress
  case object SanitizeThread
  case object SanitizeMemory
  case object Speculatable
  case class StringAttribute(
      stringAttributeKind: String,
      stringAttributeValue: String // ^ Use "" for no value // the two are conflated
  )
  case class AllocSize(a: Int, b: Option[Int]) // ^ AllocSize 0 (Just 0) is invalid
  case object WriteOnly
  case object ArgMemOnly
  case object Convergent
  case object InaccessibleMemOnly
  case object InaccessibleMemOrArgMemOnly
  case object SafeStack
}

// <http://llvm.org/docs/LangRef.html#attribute-groups>
case class GroupID(id: Int)
