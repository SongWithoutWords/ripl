// Module to allow importing 'ParameterAttribute' distinctly qualified.
package ripl.llvm.ast

// <http://llvm.org/docs/LangRef.html#parameter-attributes>
sealed trait ParameterAttribute
case object ParameterAttribute {
  case object ZeroExt extends ParameterAttribute
  case object SignExt extends ParameterAttribute
  case object InReg extends ParameterAttribute
  case object SRet extends ParameterAttribute
  case class Alignment(n: Int) extends ParameterAttribute
  case object NoAlias extends ParameterAttribute
  case object ByVal extends ParameterAttribute
  case object NoCapture extends ParameterAttribute
  case object Nest extends ParameterAttribute
  case object ReadNone extends ParameterAttribute
  case object ReadOnly extends ParameterAttribute
  case object WriteOnly extends ParameterAttribute
  case object InAlloca extends ParameterAttribute
  case object NonNull extends ParameterAttribute
  case class Dereferenceable(n: Int) extends ParameterAttribute
  case class DereferenceableOrNull(n: Int) extends ParameterAttribute
  case object Returned extends ParameterAttribute
  case object SwiftSelf extends ParameterAttribute
  case object SwiftError extends ParameterAttribute
  case class StringAttribute(
      stringAttributeKind: String,
      stringAttributeValue: String // ^ Use "" for no value - the two are conflated extends ParameterAttribute
  ) extends ParameterAttribute
}
