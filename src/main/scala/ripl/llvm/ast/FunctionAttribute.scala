// Module to allow importing 'FunctionAttribute' distinctly qualified.
package ripl.llvm.ast

// <http://llvm.org/docs/LangRef.html#function-attributes>
sealed trait FunctionAttribute
case object FunctionAttribute {
  case object NoReturn              extends FunctionAttribute
  case object NoUnwind              extends FunctionAttribute
  case object ReadNone              extends FunctionAttribute
  case object ReadOnly              extends FunctionAttribute
  case object NoInline              extends FunctionAttribute
  case object NoRecurse             extends FunctionAttribute
  case object AlwaysInline          extends FunctionAttribute
  case object MinimizeSize          extends FunctionAttribute
  case object OptimizeForSize       extends FunctionAttribute
  case object OptimizeNone          extends FunctionAttribute
  case object StackProtect          extends FunctionAttribute
  case object StackProtectReq       extends FunctionAttribute
  case object StackProtectStrong    extends FunctionAttribute
  case object NoRedZone             extends FunctionAttribute
  case object NoImplicitFloat       extends FunctionAttribute
  case object Naked                 extends FunctionAttribute
  case object InlineHint            extends FunctionAttribute
  case class StackAlignment(n: Int) extends FunctionAttribute
  case object ReturnsTwice          extends FunctionAttribute
  case object UWTable               extends FunctionAttribute
  case object NonLazyBind           extends FunctionAttribute
  case object Builtin               extends FunctionAttribute
  case object NoBuiltin             extends FunctionAttribute
  case object Cold                  extends FunctionAttribute
  case object JumpTable             extends FunctionAttribute
  case object NoDuplicate           extends FunctionAttribute
  case object SanitizeAddress       extends FunctionAttribute
  case object SanitizeThread        extends FunctionAttribute
  case object SanitizeMemory        extends FunctionAttribute
  case object SanitizeHWAddress     extends FunctionAttribute
  case object Speculatable          extends FunctionAttribute
  case class StringAttribute(
      stringAttributeKind: String,
      stringAttributeValue: String // ^ Use "" for no value // the two are conflated
    ) extends FunctionAttribute
  // ^ AllocSize 0 (Just 0) is invalid
  case class AllocSize(a: Int, b: Option[Int]) extends FunctionAttribute
  case object WriteOnly                        extends FunctionAttribute
  case object ArgMemOnly                       extends FunctionAttribute
  case object Convergent                       extends FunctionAttribute
  case object InaccessibleMemOnly              extends FunctionAttribute
  case object InaccessibleMemOrArgMemOnly      extends FunctionAttribute
  case object SafeStack                        extends FunctionAttribute
  case object StrictFP                         extends FunctionAttribute
}

// <http://llvm.org/docs/LangRef.html#attribute-groups>
case class GroupID(id: Int)
