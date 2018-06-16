// Module to allow importing 'CallingConvention' distinctly qualified.

package ripl.llvm.ast

//  <http://llvm.org/docs/LangRef.html#callingconv>
sealed trait CallingConvention
case object CallingConvention {
  case object C extends CallingConvention
  case object Fast extends CallingConvention
  case object Cold extends CallingConvention
  case object GHC extends CallingConvention
  case object HiPE extends CallingConvention
  case object WebKit_JS extends CallingConvention
  case object AnyReg extends CallingConvention
  case object PreserveMost extends CallingConvention
  case object PreserveAll extends CallingConvention
  case object Swift extends CallingConvention
  case object CXX_FastTLS extends CallingConvention
  case object X86_StdCall extends CallingConvention
  case object X86_FastCall extends CallingConvention
  case object ARM_APCS extends CallingConvention
  case object ARM_AAPCS extends CallingConvention
  case object ARM_AAPCS_VFP extends CallingConvention
  case object MSP430_INTR extends CallingConvention
  case object X86_ThisCall extends CallingConvention
  case object PTX_Kernel extends CallingConvention
  case object PTX_Device extends CallingConvention
  case object SPIR_FUNC extends CallingConvention
  case object SPIR_KERNEL extends CallingConvention
  case object Intel_OCL_BI extends CallingConvention
  case object X86_64_SysV extends CallingConvention
  case object Win64 extends CallingConvention
  case object X86_VectorCall extends CallingConvention
  case object HHVM extends CallingConvention
  case object HHVM_C extends CallingConvention
  case object X86_Intr extends CallingConvention
  case object AVR_Intr extends CallingConvention
  case object AVR_Signal extends CallingConvention
  case object AVR_Builtin extends CallingConvention
  case object AMDGPU_VS extends CallingConvention
  case object AMDGPU_HS extends CallingConvention
  case object AMDGPU_GS extends CallingConvention
  case object AMDGPU_PS extends CallingConvention
  case object AMDGPU_CS extends CallingConvention
  case object AMDGPU_Kernel extends CallingConvention
  case object X86_RegCall extends CallingConvention
  case object MSP430_Builtin extends CallingConvention
  case class Numbered(number: Int) extends CallingConvention
}
