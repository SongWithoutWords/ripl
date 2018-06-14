package ripl.llvm.pretty

import scala.util.matching.Regex

import ripl.llvm.pure.ast._

// -------------------------------------------------------------------------------
// // Utils
// -------------------------------------------------------------------------------

case object Util {

  def dquotes(s: String) = "\"" + s + "\""

  def parens(s: String) = s"($s)"

  def parensIf(b: Boolean, s: String): String = if (b) parens(s) else s

  def lines(in: List[String]): String = in.mkString("", "\n", "")

  def spaces(in: List[String]): String = in.mkString("", " ", "")
  def spaces(in: String*): String = spaces(in: _*)

  def commas(in: List[String]): String = in.mkString("", ", ", "")
  def commas(in: String*): String = commas(in: _*)

  def colons(in: List[String]): String = in.mkString("", ":", "")

  def hlinecat(in: List[String]): String = in.mkString("", "\n", "")

  def wrapbraces(leadIn: String, x: String): String = s"$leadIn {\n${x}\n}"

  def indent(n: Int, s: String) = {
    val beginningOfLine = "^".r
    beginningOfLine.replaceAllIn(s, " " * n)
  }

  def brackets(x: String): String = s"[$x]"

  def angleBrackets(x: String): String = s"<$x>"

  def braces(x: String): String = s"{$x}"
  def spacedBraces(x: String): String = s"{ $x }"

  def local(x: String): String = s"%$x"

  def global(x: String): String = s"@$x"

  def label(x: String): String = s"label %$x"

  def comma(a: String, b: String) = s"$a, $b"

  implicit class StringExtensions(lhs: String) {
    def <>(rhs: String) = lhs + rhs
    def <+>(rhs: String) = lhs + " " + rhs
    def </>(rhs: String) = lhs + "\n" + rhs
    def comma(rhs: String) = lhs + ", " + rhs
  }
}

import Util._

// -------------------------------------------------------------------------------
// Classes
// -------------------------------------------------------------------------------

case object prettyPrint {

  def pp(s: String, b: Boolean): String = if (b) s else ""

// XXX: horrible hack
// unShort :: BS.ShortByteString -> [Char]
// unShort xs = fmap (toEnum . fromIntegral) $ BS.unpack xs

// short :: BS.ShortByteString -> Doc
// short x = string (pack (unShort x))

// decodeShortUtf8 :: SBF.ShortByteString -> Text
// decodeShortUtf8 = decodeUtf8 . fromStrict . SBF.fromShort

// instance PP Word8 where
//   pp x = int (fromIntegral x)

// instance PP Word16 where
//   pp x = int (fromIntegral x)

// instance PP Word32 where
//   pp x = int (fromIntegral x)

// instance PP Word64 where
//   pp x = int (fromIntegral x)

// instance PP Int32 where
//   pp x = int (fromIntegral x)

// instance PP Int64 where
//   pp x = int (fromIntegral x)

// instance PP Integer where
//   pp = integer

// instance PP BS.ShortByteString where
//   pp = pp . unShort

// instance PP [Char] where
//   pp = text . pack

// instance PP Bool where
//   pp True = "true"
//   pp False = "false"

  def pp(b: Boolean): String = b match {
    case true  => "true"
    case false => "false"
  }

  def pp(n: Name): String = {
    def isFirst(c: Char) =
      c.isLetter || c == '-' || c == '_' || c == '$' || c == '.'
    def isRest(c: Char) = c.isDigit || isFirst(c)
    n match {
      case Name("")                                       => "\"\""
      case Name(n) if isFirst(n.head) && n.forall(isRest) => n
      case _                                              => dquotes(n.s)
    }
  }

  def pp(p: Parameter): String =
    pp(p.t) <+> ppParamAttrs(p.attributes) <+> local(pp(p.name))

  def ppParamAttrs(ps: List[ParameterAttribute]): String = spaces(ps.map(pp))

  def pp(ps: List[Parameter], variadic: Boolean): String =
    variadic match {
      case false => commas(ps.map(pp))
    }

  def pp(arg: Argument): String =
    pp(typeOf(arg.op)) <+> ppParamAttrs(arg.attrs) <+> pp(arg.op)

  def pp(addr: UnnamedAddr): String =
    addr match {
      case LocalAddr  => "local_unnamed_addr"
      case GlobalAddr => "unnamed_addr"
    }

  def pp(t: Type): String =
    t match {
      case IntegerType(width) => "i" <> width.toString
      case HalfFP             => "half"
      case FloatFP            => "float"
      case DoubleFP           => "double"
      case FP128FP            => "fp128"
      case X86_FP80FP         => "x86_fp80"
      case PPC_FP128FP        => "ppc_fp128"

      case VoidType => "void"
      case PointerType(t, AddrSpace(addr)) =>
        addr match {
          case 0 => pp(t) <> "*"
          case _ => pp(t) <+> "addrspace" <> parens(addr.toString) <> "*"
        }

      case f @ FunctionType(resultType, argumentTypes, isVarArg) =>
        pp(resultType) <+> pp(f)
      case VectorType(elCount, elType) =>
        angleBrackets(elCount.toString() <+> "x" <+> pp(elType))
      case StructureType(isPacked, elementTypes) =>
        val contents = commas(elementTypes.map(pp))
        isPacked match {
          case true  => "<{" <> contents <> "}>"
          case false => "{" <> contents <> "}"
        }
      case ArrayType(elCount, elType) =>
        brackets(elCount.toString <+> "x" <+> pp(elType))
      case NamedTypeReference(name) => "%" <> pp(name)
      case MetadataType             => "metadata"
      case TokenType                => "token"
      case LabelType                => "label"
    }

  def pp(g: Global): String = {
    g match {
      case f: Function =>
        val infoBeforeParams = spaces(
          pp(f.linkage),
          pp(f.callingConvention),
          ppParamAttrs(f.returnAttributes),
          pp(f.returnType),
          global(pp(f.name))
        )
        val infoAfterParams = spaces(
          spaces(f.functionAttributes.map(pp)),
          f.alignment match {
            case 0     => ""
            case align => "align" <+> align.toString
          },
          f.garbageCollectorName match {
            case None    => ""
            case Some(n) => "gc" <+> dquotes(n)
          },
          f.prefix match {
            case None    => ""
            case Some(p) => "prefix" <+> ppTyped(p)
          }
        )
        f.basicBlocks match {
          case Nil =>
            "declare" <+>
              infoBeforeParams <>
              ppParams(f.parameters.params.map { p: Parameter =>
                pp(typeOf(p))
              }, f.parameters.isVarArg) <+>
              infoAfterParams

          // TODO: special case for single unnamed block

          case bs =>
            "define" <+>
              infoBeforeParams <>
              ppParams(f.parameters.params.map(pp), f.parameters.isVarArg) <+>
              infoAfterParams
        }

      case g: GlobalVariable =>
        global(pp(g.name)) <+>
          "=" <+>
          ppLinkage(g.initializer.nonEmpty, g.linkage) <+>
          (g.unnamedAddr match {
            case None       => ""
            case Some(addr) => pp(addr)
          }) <+>
          (g.addrSpace match {
            case AddrSpace(0) => ""
            case AddrSpace(n) => "addrspace" <> parens(n.toString)
          }) <+>
          (g.isConstant match {
            case true  => "constant"
            case false => "global"
          }) <+>
          pp(g.t) <+>
          (g.initializer match {
            case None       => ""
            case Some(init) => pp(init)
          }) <>
          ppAlign(g.alignment)

      case g: GlobalAlias =>
        global(pp(g.name)) <+>
          "=" <+>
          pp(g.linkage) <+>
          (g.unnamedAddr match {
            case None       => ""
            case Some(addr) => pp(addr)
          }) <+>
          "alias" <+>
          pp(g.t) comma
          ppTyped(g.aliasee)
    }
  }

  def pp(om: Option[Metadata]): String = om match {
    case None    => "null"
    case Some(m) => pp(m)
  }

  def pp(d: Definition): String = d match {

    case GlobalDefinition(x) => pp(x)

    case TypeDefinition(nm, ty) =>
      local(pp(nm)) <+> "=" <+> "type" <+> ty.map(pp).getOrElse("opaque")

    case FunctionAttributes(gid, attrs) =>
      "attributes" <+> pp(gid) <+> "=" <+> braces(
        spaces(attrs.map(ppAttrInGroup))
      )
    case NamedMetadataDefinition(nm, meta) =>
      "!" <> nm <+> "=" <+> "!" <> braces(commas(meta.map(pp)))

    case MetadataNodeDefinition(node, meta) =>
      pp(node) <+> "=" <+> "!" <> braces(commas(meta.map(pp)))

    case ModuleInlineAssembly(asm) => "module asm" <+> dquotes(asm)

    case COMDAT(name, selKind) =>
      "$" <> name <+> "=" <+> "comdat" <+> pp(selKind)
  }

  def pp(s: SelectionKind): String = s match {
    case SelectionKind.Any          => "any"
    case SelectionKind.ExactMatch   => "exactmatch"
    case SelectionKind.Largest      => "largest"
    case SelectionKind.NoDuplicates => "noduplicates"
    case SelectionKind.SameSize     => "samesize"
  }

  def ppAttrInGroup(a: FunctionAttribute): String = a match {
    case FunctionAttribute.StackAlignment(n) => "alignstack=" <> pp(a)
    case attr                                => pp(attr)
  }

  def pp(fa: FunctionAttribute): String = {
    import FunctionAttribute._
    fa match {
      case NoReturn            => "noreturn"
      case NoUnwind            => "nounwind"
      case ReadNone            => "readnone"
      case ReadOnly            => "readonly"
      case WriteOnly           => "writeonly"
      case NoInline            => "noinline"
      case AlwaysInline        => "alwaysinline"
      case MinimizeSize        => "minsize"
      case OptimizeForSize     => "optsize"
      case OptimizeNone        => "optnone"
      case SafeStack           => "safestack"
      case StackProtect        => "ssp"
      case StackProtectReq     => "sspreq"
      case StackProtectStrong  => "sspstrong"
      case NoRedZone           => "noredzone"
      case NoImplicitFloat     => "noimplicitfloat"
      case Naked               => "naked"
      case InlineHint          => "inlinehint"
      case StackAlignment(n)   => "alignstack" <> parens(n.toString)
      case ReturnsTwice        => "returns_twice"
      case UWTable             => "uwtable"
      case NonLazyBind         => "nonlazybind"
      case Builtin             => "builtin"
      case NoBuiltin           => "nobuiltin"
      case Cold                => "cold"
      case JumpTable           => "jumptable"
      case NoDuplicate         => "noduplicate"
      case SanitizeAddress     => "sanitize_address"
      case SanitizeThread      => "sanitize_thread"
      case SanitizeMemory      => "sanitize_memory"
      case SanitizeHWAddress   => "sanitize_hwaddress"
      case NoRecurse           => "norecurse"
      case Convergent          => "convergent"
      case ArgMemOnly          => "argmemonly"
      case InaccessibleMemOnly => "inaccessiblememonly"
      case AllocSize(a, None)  => "allocsize" <> parens(a.toString)
      case AllocSize(a, Some(b)) =>
        "allocsize" <> parens(a.toString comma b.toString)
      case InaccessibleMemOrArgMemOnly => "inaccessiblemem_or_argmemonly"
      case StringAttribute(k, v)       => dquotes(k) <> "=" <> dquotes(v)
      case Speculatable                => "speculatable"
      case StrictFP                    => "strictfp"
    }
  }

  def pp(a: ParameterAttribute): String = {
    import ParameterAttribute._
    a match {
      case ZeroExt            => "zeroext"
      case SignExt            => "signext"
      case InReg              => "inreg"
      case SRet               => "sret"
      case Alignment(n)       => "align" <+> n.toString
      case NoAlias            => "noalias"
      case ByVal              => "byval"
      case NoCapture          => "nocapture"
      case Nest               => "nest"
      case ReadNone           => "readnone"
      case ReadOnly           => "readonly"
      case WriteOnly          => "writeonly"
      case InAlloca           => "inalloca"
      case NonNull            => "nonnull"
      case Dereferenceable(n) => "dereferenceable" <> parens(n.toString)
      case DereferenceableOrNull(n) =>
        "dereferenceable_or_null" <> parens(n.toString)
      case Returned              => "returned"
      case SwiftSelf             => "swiftself"
      case SwiftError            => "swifterror"
      case StringAttribute(k, v) => dquotes(k) <> "=" <> dquotes(v)
    }
  }

  def pp(c: CallingConvention): String = {
    import CallingConvention._
    c match {
      case Numbered(n)    => "cc" <+> n.toString
      case C              => "ccc"
      case Fast           => "fastcc"
      case Cold           => "coldcc"
      case GHC            => "cc 10"
      case HiPE           => "cc 11"
      case WebKit_JS      => "webkit_jscc"
      case AnyReg         => "anyregcc"
      case PreserveMost   => "preserve_mostcc"
      case PreserveAll    => "preserve_allcc"
      case Swift          => "swiftcc"
      case CXX_FastTLS    => "cxx_fast_tlscc"
      case X86_StdCall    => "cc 64"
      case X86_FastCall   => "cc 65"
      case ARM_APCS       => "cc 66"
      case ARM_AAPCS      => "cc 67"
      case ARM_AAPCS_VFP  => "cc 68"
      case MSP430_INTR    => "cc 69"
      case X86_ThisCall   => "cc 70"
      case PTX_Kernel     => "cc 71"
      case PTX_Device     => "cc 72"
      case SPIR_FUNC      => "cc 75"
      case SPIR_KERNEL    => "cc 76"
      case Intel_OCL_BI   => "cc 77"
      case X86_64_SysV    => "cc 78"
      case Win64          => "cc 79"
      case X86_Intr       => "x86_intrcc"
      case X86_RegCall    => "x86_regcallcc"
      case X86_VectorCall => "x86_vectorcallcc"
      case AVR_Intr       => "avr_intrcc"
      case AVR_Signal     => "avr_signalcc"
      case AVR_Builtin    => "cc 86"
      case HHVM           => "hhvmcc"
      case HHVM_C         => "hhvm_ccc"
      case AMDGPU_VS      => "amdgpu_vs"
      case AMDGPU_GS      => "amdgpu_gs"
      case AMDGPU_PS      => "amdgpu_ps"
      case AMDGPU_CS      => "amdgpu_cs"
      case AMDGPU_HS      => "amdgpu_hs"
      case AMDGPU_Kernel  => "amdgpu_kernel"
      case MSP430_Builtin => "msp430"
    }
  }

  def pp(l: Linkage): String = ppLinkage(false, l)

  def ppLinkage(omitExternal: Boolean, l: Linkage): String = {
    import Linkage._
    l match {
      case External            => if (omitExternal) "" else "external"
      case Private             => "private"
      case Internal            => "internal"
      case ExternWeak          => "extern_weak"
      case AvailableExternally => "available_externally"
      case LinkOnce            => "linkonce"
      case Weak                => "weak"
      case Common              => "common"
      case Appending           => "appending"
      case LinkOnceODR         => "linkonce_odr"
      case WeakODR             => "weak_odr"
    }
  }

  def pp(m: InstructionMetadata): String =
    commas(m.data.map { case (x, y) => "!" <> x <+> pp(y) })

  def pp(m: MetadataNodeID): String = {
    val MetadataNodeID(id) = m
    "!" <> id.toString
  }

  def pp(gid: GroupID): String = {
    val GroupID(id) = gid
    "#" <> gid.toString
  }

  def pp(b: BasicBlock): String =
    (b.name match {
      case Name(n) => n <> ":"
    }) </>
      indent(
        2,
        lines(
          b.instructions.map { i: Named[Instruction] =>
            pp(i.map(pp))
          } ++ List(pp(b.terminator.map(pp)))
        )
      )

  def pp(t: Terminator): String = {
    import Terminator._
    t match {
      case Br(dest, meta) => "br" <+> label(pp(dest)) <+> pp(meta)

      case Ret(value, meta) => "ret" <+> value.map(ppTyped).getOrElse("void")

      case CondBr(cond, tdest, fdest, meta) =>
        "br" <+>
          ppTyped(cond) comma
          label(pp(tdest)) comma
          label(pp(fdest)) <+>
            pp(meta)

      case Switch(op, defaultDest, dests, meta) =>
        "switch" <+>
          ppTyped(op) comma
          label(pp(defaultDest)) <+>
            brackets(spaces(dests.map {
              case (v, l) => ppTyped(v) comma label(pp(l))
            })) <+>
            pp(meta)

      case Unreachable(meta) => "unreachable" <+> pp(meta)

      case IndirectBr(op, dests, meta) =>
        "indirectbr" <+>
          ppTyped(op) comma
          brackets(spaces(dests.map { n: Name =>
            label(pp(n))
          })) <+>
            pp(meta)

      case i: Invoke =>
        val ftype = typeOf(i.function) match {
          case PointerType(ft: FunctionType, _) => ft
          case _ =>
            throw new Exception("Invoke requires function type")
        }
        "invoke" <+>
          pp(i.callingConvention) <+>
          pp(ftype.resultType) <+>
          (if (ftype.isVarArg) ppFunctionArgumentTypes(ftype) else "") <+>
          pp(i.function) <+>
          parens(commas(i.arguments.map(pp))) <+>
          pp(i.functionAttributes) <+>
          "to" <+> label(pp(i.returnDest)) <+>
          "unwind" <+> label(pp(i.exceptionDest)) <+>
          pp(i.metadata)

      case Resume(op, meta) => "resume" <+> ppTyped(op) <+> pp(meta)

      case CleanupRet(pad, dest, meta) =>
        "cleanupret" <+> "from" <+> pp(pad) <+> "unwind" <+> dest
          .map { n: Name =>
            label(pp(n))
          }
          .getOrElse("to caller")

      case CatchRet(pad, successor, meta) =>
        "catchret" <+> "from" <+> pp(pad) <+> "to" <+> label(pp(successor)) <+>
          pp(meta)

      case CatchSwitch(parentPad, catchHandlers, defaultUnwindDest, metadata) =>
        "catchswitch" <+>
          "within" <+>
          pp(parentPad) <+>
          brackets(commas(catchHandlers.toList.map { n: Name =>
            label(pp(n))
          })) <+>
          "unwind" <+> "to" <+> defaultUnwindDest
          .map(pp)
          .getOrElse("caller") <+>
          pp(metadata)
    }
  }

  def pp(i: Instruction): String = ???
// instance PP Instruction where
//   pp = \case
//     Add {..}    -> "add"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     Sub {..}    -> "sub"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     Mul {..}    -> "mul"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     Shl {..}    -> "shl"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     AShr {..}   -> "ashr" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     LShr {..}   -> "lshr" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     And {..}    -> "and"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     Or {..}     -> "or"   <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     Xor {..}    -> "xor"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     SDiv {..}   -> "sdiv"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     UDiv {..}   -> "udiv"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     SRem {..}   -> "srem"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     URem {..}   -> "urem"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata

//     FAdd {..}   -> "fadd" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     FSub {..}   -> "fsub" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     FMul {..}   -> "fmul" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     FDiv {..}   -> "fdiv" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     FRem {..}   -> "frem" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
//     FCmp {..}   -> "fcmp" <+> pp fpPredicate <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata

//     Alloca {..} -> "alloca" <+> pp allocatedType <> num <> ppAlign alignment <+> ppInstrMeta metadata
//       where num   = case numElements of Nothing -> empty
//                                         Just o -> "," <+> ppTyped o
//     Store {..}  -> "store" <+> ppTyped value `cma` ppTyped address <> ppAlign alignment
//     Load {..}   -> "load" <+> pp argTy `cma` ppTyped address <> ppAlign alignment <+> ppInstrMeta metadata
//       where PointerType argTy _ = typeOf address
//     Phi {..}    -> "phi" <+> pp type' <+> commas (fmap phiIncoming incomingValues) <+> ppInstrMeta metadata

//     ICmp {..}   -> "icmp" <+> pp iPredicate <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata

//     c@Call {..} -> ppCall c  <+> ppInstrMeta metadata
//     Select {..} -> "select" <+> commas [ppTyped condition', ppTyped trueValue, ppTyped falseValue] <+> ppInstrMeta metadata
//     SExt {..}   -> "sext" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
//     ZExt {..}   -> "zext" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
//     FPExt {..}   -> "fpext" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
//     Trunc {..}  -> "trunc" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
//     FPTrunc {..}  -> "fptrunc" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata

//     GetElementPtr {..} -> "getelementptr" <+> bounds inBounds <+> commas (pp argTy : fmap ppTyped (address:indices)) <+> ppInstrMeta metadata
//       where argTy = getElementType $ typeOf address
//     ExtractValue {..} -> "extractvalue" <+> commas (ppTyped aggregate : fmap pp indices') <+> ppInstrMeta metadata

//     BitCast {..} -> "bitcast" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
//     FPToUI {..} -> "fptoui" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
//     FPToSI {..} -> "fptosi" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
//     UIToFP {..} -> "uitofp" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
//     SIToFP {..} -> "sitofp" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
//     PtrToInt {..} -> "ptrtoint" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
//     IntToPtr {..} -> "inttoptr" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata

//     InsertElement {..} -> "insertelement" <+> commas [ppTyped vector, ppTyped element, ppTyped index] <+> ppInstrMeta metadata
//     ShuffleVector {..} -> "shufflevector" <+> commas [ppTyped operand0, ppTyped operand1, ppTyped mask] <+> ppInstrMeta metadata
//     ExtractElement {..} -> "extractelement" <+> commas [ppTyped vector, ppTyped index] <+> ppInstrMeta metadata
//     InsertValue {..} -> "insertvalue" <+> commas (ppTyped aggregate : ppTyped element : fmap pp indices') <+> ppInstrMeta metadata

//     Fence {..} -> "fence" <+> pp atomicity <+> ppInstrMeta metadata
//     AtomicRMW {..} -> "atomicrmw" <+> ppVolatile volatile <+> pp rmwOperation <+> ppTyped address `cma` ppTyped value <+> pp atomicity  <+> ppInstrMeta metadata
//     CmpXchg {..} -> "cmpxchg" <+> ppVolatile volatile <+> ppTyped address `cma` ppTyped expected `cma` ppTyped replacement
//       <+> pp atomicity <+> pp failureMemoryOrdering <+> ppInstrMeta metadata

//     AddrSpaceCast {..} -> "addrspacecast" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
//     VAArg {..} -> "va_arg" <+> ppTyped argList `cma` pp type' <+> ppInstrMeta metadata

//     LandingPad {..} ->
//       "landingpad" <+> pp type' <+> ppBool "cleanup" cleanup <+> ppInstrMeta metadata
//       <+> commas (fmap pp clauses)
//     CatchPad {..} -> "catchpad" <+> "within" <+> pp catchSwitch <+> brackets (commas (map ppTyped args)) <+> ppInstrMeta metadata
//     CleanupPad {..} -> "cleanuppad" <+> "within" <+> pp parentPad <+> brackets (commas (map ppTyped args)) <+> ppInstrMeta metadata

//     where
//       bounds True = "inbounds"
//       bounds False = empty

  def pp(c: CallableOperand): String = ???
// instance PP CallableOperand where
//   pp (Left asm) = error "CallableOperand"
//   pp (Right op) = pp op

  def pp(l: LandingPadClause): String = ???
// instance PP LandingPadClause where
//   pp = \case
//     Catch c  -> "catch" <+> ppTyped c
//     Filter c -> "filter" <+> ppTyped c

  def pp(e: List[Either[GroupID, FunctionAttribute]]): String = ???
// instance PP [Either GroupID FunctionAttribute] where
//   pp x = hsep $ fmap pp x

  def pp(e: Either[GroupID, FunctionAttribute]): String = ???
// instance PP (Either GroupID FunctionAttribute) where
//   pp (Left gid) = pp gid
//   pp (Right fattr) = pp fattr

  def pp(op: Operand): String = ???
// instance PP Operand where
//   pp (LocalReference _ nm) = local' (pp nm)
//   pp (ConstantOperand con) = pp con
//   pp (MetadataOperand mdata) = pp mdata

  def pp(m: Metadata): String =
    m match {
      case MDString(str)    => "!" <> dquotes(str)
      case MDNode(node)     => pp(node)
      case MDValue(operand) => pp(operand)
    }

  def pp(m: MetadataNode): String =
    m match {
      case MetadataNodeData(xs)       => "!" <> braces(commas(xs.map(pp)))
      case MetadataNodeReference(ref) => pp(ref)
    }

  def pp(c: Constant): String = ???
// instance PP C.Constant where
//   pp (C.Int width val) = pp val
//   pp (C.Float (F.Double val))      =
//     if specialFP val
//       then "0x" <> (text . pack) (showHex (doubleToWord val) "")
//       else text $ pack $ printf "%6.6e" val
//   pp (C.Float (F.Single val))      =
//     if specialFP val
//       then "0x" <> (text . pack) (showHex (floatToWord val) "")
//       else text $ pack $ printf "%6.6e" val
//   pp (C.Float (F.Half val))        = text $ pack $ printf "%6.6e" val
//   pp (C.Float (F.Quadruple val _)) = text $ pack $ printf "%6.6e" val
//   pp (C.Float (F.X86_FP80 val _))  = text $ pack $ printf "%6.6e" val
//   pp (C.Float (F.PPC_FP128 val _)) = text $ pack $ printf "%6.6e" val

//   pp (C.GlobalReference ty nm) = "@" <> pp nm
//   pp (C.Vector args) = "<" <+> commas (fmap ppTyped args) <+> ">"

//   pp (C.Add {..})    = "add"  <+> ppTyped operand0 `cma` pp operand1
//   pp (C.Sub {..})    = "sub"  <+> ppTyped operand0 `cma` pp operand1
//   pp (C.Mul {..})    = "mul"  <+> ppTyped operand0 `cma` pp operand1
//   pp (C.Shl {..})    = "shl"  <+> ppTyped operand0 `cma` pp operand1
//   pp (C.AShr {..})   = "ashr" <+> ppTyped operand0 `cma` pp operand1
//   pp (C.LShr {..})   = "lshr" <+> ppTyped operand0 `cma` pp operand1
//   pp (C.And {..})    = "and"  <+> ppTyped operand0 `cma` pp operand1
//   pp (C.Or {..})     = "or"   <+> ppTyped operand0 `cma` pp operand1
//   pp (C.Xor {..})    = "xor"  <+> ppTyped operand0 `cma` pp operand1
//   pp (C.SDiv {..})   = "sdiv"  <+> ppTyped operand0 `cma` pp operand1
//   pp (C.UDiv {..})   = "udiv"  <+> ppTyped operand0 `cma` pp operand1
//   pp (C.SRem {..})   = "srem"  <+> ppTyped operand0 `cma` pp operand1
//   pp (C.URem {..})   = "urem"  <+> ppTyped operand0 `cma` pp operand1

//   pp (C.FAdd {..})   = "fadd" <+> ppTyped operand0 `cma` pp operand1
//   pp (C.FSub {..})   = "fsub" <+> ppTyped operand0 `cma` pp operand1
//   pp (C.FMul {..})   = "fmul" <+> ppTyped operand0 `cma` pp operand1
//   pp (C.FDiv {..})   = "fdiv" <+> ppTyped operand0 `cma` pp operand1
//   pp (C.FRem {..})   = "frem" <+> ppTyped operand0 `cma` pp operand1
//   pp (C.FCmp {..})   = "fcmp" <+> pp fpPredicate <+> ppTyped operand0 `cma` pp operand1
//   pp C.ICmp {..}     = "icmp" <+> pp iPredicate <+> ppTyped operand0 `cma` pp operand1

//   pp (C.Select {..})  = "select" <+> commas [ppTyped condition', ppTyped trueValue, ppTyped falseValue]
//   pp (C.SExt {..})    = "sext" <+> ppTyped operand0 <+> "to" <+> pp type'
//   pp (C.ZExt {..})    = "zext" <+> ppTyped operand0 <+> "to" <+> pp type'
//   pp (C.FPExt {..})   = "fpext" <+> ppTyped operand0 <+> "to" <+> pp type'
//   pp (C.Trunc {..})   = "trunc" <+> ppTyped operand0 <+> "to" <+> pp type'
//   pp (C.FPTrunc {..}) = "fptrunc" <+> ppTyped operand0 <+> "to" <+> pp type'

//   pp C.FPToUI {..} = "fptoui" <+> ppTyped operand0 <+> "to" <+> pp type'
//   pp C.FPToSI {..} = "fptosi" <+> ppTyped operand0 <+> "to" <+> pp type'
//   pp C.UIToFP {..} = "uitofp" <+> ppTyped operand0 <+> "to" <+> pp type'
//   pp C.SIToFP {..} = "sitofp" <+> ppTyped operand0 <+> "to" <+> pp type'

//   pp (C.Struct _ packed elems) =
//     let struct = spacedbraces $ commas $ fmap ppTyped elems
//     in if packed
//          then angleBrackets struct
//          else struct

//   pp (C.Null constantType) = ppNullInitializer constantType

// #if MIN_VERSION_llvm_hs_pure(5,1,3)
//   pp (C.AggregateZero constantType) = "zeroinitializer"
// #endif

//   pp (C.Undef {}) = "undef"
//   pp (C.TokenNone {}) = "none"
//   pp (C.BlockAddress fn blk) = "blockaddress" <> parens (commas (fmap pp [fn, blk]))

//   pp C.Array {..}
//     | memberType == (IntegerType 8) = "c" <> (dquotes $ hcat [ppIntAsChar val | C.Int _ val <- memberValues])
//     | otherwise = brackets $ commas $ fmap ppTyped memberValues

//   pp C.GetElementPtr {..} = "getelementptr" <+> bounds inBounds <+> parens (commas (pp argTy : fmap ppTyped (address:indices)))
//     where
//       PointerType argTy _ = typeOf address
//       bounds True = "inbounds"
//       bounds False = empty

//   pp C.BitCast {..} = "bitcast" <+> parens (ppTyped operand0 <+> "to" <+> pp type')
//   pp C.PtrToInt {..} = "ptrtoint" <+> parens (ppTyped operand0 <+> "to" <+> pp type')
//   pp C.IntToPtr {..} = "inttoptr" <+> parens (ppTyped operand0 <+> "to" <+> pp type')
//   pp C.AddrSpaceCast {..} = "addrspacecast" <+> parens (ppTyped operand0 <+> "to" <+> pp type')
//   pp _ = error "Non-function argument. (Malformed AST)"

  def pp(n: Named[String]): String = n match {
    case n := s => "%" <> pp(n) <+> "=" <+> s
    case Do(s)  => s
  }

// instance PP a => PP (Named a) where
//   pp (nm := a) = "%" <> pp nm <+> "=" <+> pp a
//   pp (Do a) = pp a

  def pp(m: Module): String = ???
// instance PP Module where
//   pp Module {..} =
//     let header = printf "; ModuleID = '%s'" (unShort moduleName) in
//     let target = case moduleTargetTriple of
//                       Nothing -> mempty
//                       Just target -> "target triple =" <+> dquotes (pp target) in
//     let layout = case moduleDataLayout of
//                       Nothing     -> mempty
//                       Just layout -> "target datalayout =" <+> dquotes (pp layout) in
//     hlinecat (fromString header : (layout </> target) : (fmap pp moduleDefinitions))

  def pp(fp: FloatingPointPredicate): String = ???
// instance PP FP.FloatingPointPredicate where
//   pp op = case op of
//    FP.False -> "false"
//    FP.OEQ   -> "oeq"
//    FP.OGT   -> "ogt"
//    FP.OGE   -> "oge"
//    FP.OLT   -> "olt"
//    FP.OLE   -> "ole"
//    FP.ONE   -> "one"
//    FP.ORD   -> "ord"
//    FP.UEQ   -> "ueq"
//    FP.UGT   -> "ugt"
//    FP.UGE   -> "uge"
//    FP.ULT   -> "ult"
//    FP.ULE   -> "ule"
//    FP.UNE   -> "une"
//    FP.UNO   -> "uno"
//    FP.True  -> "true"

  def pp(ip: IntegerPredicate): String = ???
// instance PP IP.IntegerPredicate where
//   pp op = case op of
//    IP.EQ  -> "eq"
//    IP.NE  -> "ne"
//    IP.UGT -> "ugt"
//    IP.UGE -> "uge"
//    IP.ULT -> "ult"
//    IP.ULE -> "ule"
//    IP.SGT -> "sgt"
//    IP.SGE -> "sge"
//    IP.SLT -> "slt"
//    IP.SLE -> "sle"

  def pp(a: Atomicity): String = ???
// instance PP Atomicity where
//   pp (scope, order) =
//     pp scope <+> pp order

  def pp(s: SynchronizationScope): String = ???
// instance PP SynchronizationScope where
//   pp = \case
//     SingleThread -> "syncscope(\"singlethread\")"
//     System -> mempty

  def pp(mo: MemoryOrdering): String = ???
// instance PP MemoryOrdering where
//   pp = \case
//     Unordered              -> "unordered"
//     Monotonic              -> "monotonic"
//     Acquire                -> "acquire"
//     Release                -> "release"
//     AcquireRelease         -> "acq_rel"
//     SequentiallyConsistent -> "seq_cst"

  def pp(op: RMWOperation): String = ???
// instance PP RMW.RMWOperation where
//   pp = \case
//     RMW.Xchg -> "xchg"
//     RMW.Add -> "add"
//     RMW.Sub -> "sub"
//     RMW.And -> "and"
//     RMW.Nand -> "nand"
//     RMW.Or -> "or"
//     RMW.Xor -> "xor"
//     RMW.Max -> "max"
//     RMW.Min -> "min"
//     RMW.UMax -> "umax"
//     RMW.UMin -> "umin"

  def pp(d: DataLayout): String = ???
// instance PP DataLayout where
//   pp x = pp (BL.unpack (dataLayoutToString x))

// //-----------------------------------------------------------------------------
// // Special Case Hacks
// //-----------------------------------------------------------------------------

  def escape(c: Char): String = ???
// escape :: Char -> Doc
// escape '"'  = "\\22"
// escape '\\' = "\\\\"
// escape c    = if isAscii c && not (isControl c)
//               then char c
//               else "\\" <> hex c
//     where
//         hex :: Char -> Doc
//         hex = pad0 . ($ []) . showHex . ord
//         pad0 :: String -> Doc
//         pad0 [] = "00"
//         pad0 [x] = "0" <> char x
//         pad0 xs = text (pack xs)

  def ppVolatile(b: Boolean): String = ???
// ppVolatile :: Bool -> Doc
// ppVolatile True = "volatile"
// ppVolatile False = mempty

// ppIntAsChar :: Integral a => a -> Doc
// ppIntAsChar = escape . chr . fromIntegral

  def ppAlign(a: Int): String = ???
// ppAlign :: Word32 -> Doc
// ppAlign x | x == 0    = empty
//           | otherwise = ", align" <+> pp x

  def ppTyped(a: Operand): String = ???
  def ppTyped(c: Constant): String = ???

// // print an operand and its type
// ppTyped :: (PP a, Typed a) => a -> Doc
// ppTyped a = pp (typeOf a) <+> pp a

// ppCommaTyped :: (PP a, Typed a) => a -> Doc
// ppCommaTyped a = pp (typeOf a) `cma` pp a

  def ppPhiIncoming(op: Operand, nm: Name): String =
    brackets(pp(op) comma (local(pp(nm))))

  def ppParams[A](params: List[String], isVarArg: Boolean): String =
    commas(params ++ (if (isVarArg) List("...") else Nil))

  // arguments: List[(Operand, List[ParameterAttribute])]
// ppParams :: (a -> Doc) -> ([a], Bool) -> Doc
// ppParams ppParam (ps, varrg) = parens . commas $ fmap ppParam ps ++ vargs
//     where
//         vargs = if varrg then ["..."] else []

  def ppFunctionArgumentTypes(ft: FunctionType): String =
    ppParams(ft.argumentTypes.map(pp), ft.isVarArg)

// ppFunctionArgumentTypes :: Type -> Doc
// ppFunctionArgumentTypes FunctionType {..} = ppParams pp (argumentTypes, isVarArg)
// ppFunctionArgumentTypes _ = error "Non-function argument. (Malformed AST)"

// ppNullInitializer :: Type -> Doc
// ppNullInitializer PointerType {..} = "zeroinitializer"
// ppNullInitializer StructureType {..} = "zeroinitializer"
// ppNullInitializer FunctionType {..} = "zeroinitializer"
// ppNullInitializer ArrayType {..} = "zeroinitializer"
// ppNullInitializer _ = error "Non-pointer argument. (Malformed AST)"

// ppCall :: Instruction -> Doc
// ppCall Call { function = Right f,..}
//   = tail <+> "call" <+> pp callingConvention <+> pp returnAttributes <+> pp resultType <+> ftype
//     <+> pp f <> parens (commas $ fmap pp arguments) <+> pp functionAttributes
//     where
//       (functionType@FunctionType {..}) = referencedType (typeOf f)
//       ftype = if isVarArg
//               then ppFunctionArgumentTypes functionType
//               else empty
//       referencedType (PointerType t _) = referencedType t
//       referencedType t                 = t

//       tail = case tailCallKind of
//         Just Tail -> "tail"
//         Just MustTail -> "musttail"
//         Just NoTail -> "notail"
//         Nothing -> empty
// ppCall Call { function = Left (IA.InlineAssembly {..}), ..}
//   = tail <+> "call" <+> pp callingConvention <+> pp returnAttributes <+> pp type'
//     <+> "asm" <+> sideeffect' <+> align' <+> dialect' <+> dquotes (text (pack (BL.unpack assembly))) <> ","
//     <+> dquotes (pp constraints) <> parens (commas $ fmap pp arguments) <+> pp functionAttributes
//     where
//       tail = case tailCallKind of
//         Just Tail -> "tail"
//         Just MustTail -> "musttail"
//         Just NoTail -> "notail"
//         Nothing -> empty
//       // If multiple keywords appear the ‘sideeffect‘ keyword must come first,
//       // the ‘alignstack‘ keyword second and the ‘inteldialect‘ keyword last.
//       sideeffect' = if hasSideEffects then "sideeffect" else ""
//       align' = if alignStack then "alignstack" else ""
//       // ATTDialect is assumed if not specified
//       dialect' = case dialect of IA.ATTDialect -> ""; IA.IntelDialect -> "inteldialect"
// ppCall x = error "Non-callable argument. (Malformed AST)"

// // Differs from Call in record name conventions only so needs a seperate almost
// // identical function. :(
// ppInvoke :: Terminator -> Doc
// ppInvoke Invoke { function' = Right f,..}
//   = "invoke" <+> pp callingConvention' <+> pp resultType <+> ftype
//     <+> pp f <> parens (commas $ fmap pp arguments') <+> pp functionAttributes'
//     where
//       (functionType@FunctionType {..}) = referencedType (typeOf f)
//       ftype = if isVarArg
//               then ppFunctionArgumentTypes functionType
//               else empty
//       referencedType (PointerType t _) = referencedType t
//       referencedType t                 = t
// ppInvoke x = error "Non-callable argument. (Malformed AST)"

// ppSingleBlock :: BasicBlock -> Doc
// ppSingleBlock (BasicBlock nm instrs term) = (vcat $ (fmap pp instrs) ++ [pp term])

// // According to <https://stackoverflow.com/a/7002812/3877993> this is
// // the best way to cast floats to words.

// cast :: (MArray (STUArray s) a (ST s),
//          MArray (STUArray s) b (ST s)) => a -> ST s b
// cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

// doubleToWord :: Double -> Word64
// doubleToWord x = runST (cast x)

// floatToWord :: Float -> Word32
// floatToWord x = runST (cast x)

// specialFP :: RealFloat a => a -> Bool
// specialFP f = isNaN f || f == 1 / 0 || f == - 1 / 0

// ppInstrMeta :: InstructionMetadata -> Doc
// ppInstrMeta [] = mempty
// ppInstrMeta xs = "," <> pp xs

// //-----------------------------------------------------------------------------
// // Toplevel
// //-----------------------------------------------------------------------------

// // | Pretty print a LLVM module
// ppllvm :: Module -> Text
// ppllvm = displayT . renderPretty 0.4 100 . pp

// // | Pretty print a printable LLVM expression
// ppll :: PP a => a -> Text
// ppll = displayT . renderPretty 0.4 100 . pp

}
