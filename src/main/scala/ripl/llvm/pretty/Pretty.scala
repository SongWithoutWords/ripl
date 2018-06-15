package ripl.llvm.pretty

import scala.util.matching.Regex

import ripl.llvm.pure.ast._

// -------------------------------------------------------------------------------
// // Utils
// -------------------------------------------------------------------------------

case object Util {

  def dquotes(s: String) = "\"" + s + "\""

  def singleQuotes(s: String) = "\'" + s + "\'"

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

  def ppIf(b: Boolean, s: String): String = if (b) s else ""

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

  def pp(i: Instruction): String = {
    import Instruction._
    i match {
      case i: Add =>
        "add" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: Sub =>
        "sub" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: Mul =>
        "mul" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: Shl =>
        "shl" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: AShr =>
        "ashr" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: LShr =>
        "lshr" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: And =>
        "and" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: Or =>
        "or" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: Xor =>
        "xor" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: SDiv =>
        "sdiv" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: UDiv =>
        "udiv" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: SRem =>
        "srem" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: URem =>
        "urem" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)

      case i: FAdd =>
        "fadd" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: FSub =>
        "fsub" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: FMul =>
        "fmul" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: FDiv =>
        "fdiv" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: FRem =>
        "frem" <+> ppTyped(i.operand0) comma pp(i.operand1) <+> pp(i.metadata)
      case i: FCmp =>
        "fcmp" <+> pp(i.fpPredicate) <+> ppTyped(i.operand0) comma
          pp(i.operand1) <+> pp(i.metadata)

      case i: Alloca =>
        "alloca" <+> pp(i.allocatedType) <>
          (i.numElements match {
            case None     => ""
            case Some(op) => "," <+> ppTyped(op)
          }) <>
          ppAlign(i.alignment) <+>
          pp(i.metadata)

      case i: Store =>
        "store" <+> ppTyped(i.value) comma ppTyped(i.address) <>
          ppAlign(i.alignment) <+>
          pp(i.metadata)

      case i: Load =>
        val PointerType(argTy, _) = typeOf(i.address)
        "load" <+> pp(argTy) comma ppTyped(i.address) <> ppAlign(i.alignment) <+>
          pp(i.metadata)

      case i: Phi =>
        "phi" <+> pp(i.t) <+> commas(i.incomingValues.map {
          case (op, nm) => ppPhiIncoming(op, nm)
        }) <+> pp(
          i.metadata
        )

      case i: ICmp =>
        "icmp" <+> pp(i.iPredicate) <+> ppTyped(i.operand0) comma pp(i.operand1) <+>
          pp(i.metadata)

      case i: Call => ppCall(i) <+> pp(i.metadata)

      case i: Select =>
        "select" <+> commas(
          ppTyped(i.condition),
          ppTyped(i.trueValue),
          ppTyped(i.falseValue)
        ) <+> pp(i.metadata)

      case i: SExt =>
        "sext" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+> pp(i.metadata)

      case i: ZExt =>
        "zext" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+> pp(i.metadata)

      case i: FPExt =>
        "fpext" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+> pp(i.metadata)

      case i: Trunc =>
        "trunc" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+> pp(i.metadata)

      case i: FPTrunc =>
        "fptrunc" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+>
          pp(i.metadata)

      case i: GetElementPtr =>
        val argTy = getElementType(typeOf(i.address))
        "getelementptr" <+> ppIf(i.inBounds, "inbounds") <+>
          commas(pp(argTy) :: (i.address :: i.indices).map(ppTyped)) <+>
          pp(i.metadata)

      case i: ExtractValue =>
        "extractvalue" <+>
          commas(ppTyped(i.aggregate) :: i.indices.map(_.toString)) <+>
          pp(i.metadata)

      case i: BitCast =>
        "bitcast" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+>
          pp(i.metadata)

      case i: FPToUI =>
        "fptoui" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+> pp(i.metadata)

      case i: FPToSI =>
        "fptosi" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+> pp(i.metadata)

      case i: UIToFP =>
        "uitofp" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+> pp(i.metadata)

      case i: SIToFP =>
        "sitofp" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+> pp(i.metadata)

      case i: PtrToInt =>
        "ptrtoint" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+>
          pp(i.metadata)

      case i: IntToPtr =>
        "inttoptr" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+>
          pp(i.metadata)

      case i: InsertElement =>
        "insertelement" <+> commas(
          ppTyped(i.vector),
          ppTyped(i.element),
          ppTyped(i.index)
        ) <+> pp(i.metadata)

      case i: ShuffleVector =>
        "shufflevector" <+> commas(
          ppTyped(i.operand0),
          ppTyped(i.operand1),
          ppTyped(i.mask)
        ) <+> pp(i.metadata)

      case i: ExtractElement =>
        "extractelement" <+> commas(ppTyped(i.vector), ppTyped(i.index)) <+>
          pp(i.metadata)

      case i: InsertValue =>
        "insertvalue" <+> commas(
          ppTyped(i.aggregate) ::
            ppTyped(i.element) ::
            i.indices.map(_.toString)
        ) <+> pp(i.metadata)

      case i: Fence => "fence" <+> pp(i.atomicity) <+> pp(i.metadata)
      case i: AtomicRMW =>
        "atomicrmw" <+> ppVolatile(i.volatile) <+> pp(i.rmwOperation) <+>
          ppTyped(i.address) comma ppTyped(i.value) <+> pp(i.atomicity) <+>
          pp(i.metadata)

      case i: CmpXchg =>
        "cmpxchg" <+> ppVolatile(i.volatile) <+> ppTyped(i.address) comma
          ppTyped(i.expected) comma ppTyped(i.replacement) <+>
          pp(i.atomicity) <+> pp(i.failureMemoryOrdering) <+> pp(i.metadata)

      case i: AddrSpaceCast =>
        "addrspacecast" <+> ppTyped(i.operand0) <+> "to" <+> pp(i.t) <+>
          pp(i.metadata)

      case i: VAArg =>
        "va_arg" <+> ppTyped(i.argList) comma pp(i.t) <+> pp(i.metadata)

      case i: LandingPad =>
        "landingpad" <+> pp(i.t) <+> ppIf(i.cleanup, "cleanup") <+>
          pp(i.metadata) <+>
          commas(i.clauses.map(pp))

      case i: CatchPad =>
        "catchpad" <+> "within" <+> pp(i.catchSwitch) <+>
          brackets(commas(i.args.map(ppTyped))) <+> pp(i.metadata)

      case i: CleanupPad =>
        "cleanuppad" <+> "within" <+> pp(i.parentPad) <+>
          brackets(commas(i.args.map(ppTyped))) <+> pp(i.metadata)
    }
  }

  def pp(c: CallableOperand): String = c match {
    case op: Operand        => pp(op)
    case ia: InlineAssembly => ???
  }

  def pp(l: LandingPadClause): String = l match {
    case Catch(c)  => "catch" <+> ppTyped(c)
    case Filter(c) => "filter" <+> ppTyped(c)
  }

  def pp(x: List[Either[GroupID, FunctionAttribute]]): String =
    spaces(x.map(pp))

  def pp(e: Either[GroupID, FunctionAttribute]): String = e match {
    case Left(gid)    => pp(gid)
    case Right(fattr) => pp(fattr)
  }

  def pp(op: Operand): String = op match {
    case LocalReference(_, nm) => local(pp(nm))
    case ConstantOperand(c)    => pp(c)
    case MetadataOperand(meta) => pp(meta)
  }

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

  def pp(c: Constant): String = {
    import Constant._
    c match {
      case Integral(_, value) => value.toString

      case F32(value) => "%6.6e".format(value)
      case F64(value) => "%6.6e".format(value)

      case GlobalReference(_, nm) => global(pp(nm))
      case Vector(members)        => angleBrackets(commas(members.map(ppTyped)))

      case Add(nuw, nsw, op0, op1) => "add" <+> ppTyped(op0) comma pp(op1)
      case Sub(nuw, nsw, op0, op1) => "sub" <+> ppTyped(op0) comma pp(op1)
      case Mul(nuw, nsw, op0, op1) => "mul" <+> ppTyped(op0) comma pp(op1)
      case Shl(nuw, nsw, op0, op1) => "shl" <+> ppTyped(op0) comma pp(op1)
      case AShr(exact, op0, op1)   => "ashr" <+> ppTyped(op0) comma pp(op1)
      case LShr(exact, op0, op1)   => "lshr" <+> ppTyped(op0) comma pp(op1)
      case And(op0, op1)           => "and" <+> ppTyped(op0) comma pp(op1)
      case Or(op0, op1)            => "or" <+> ppTyped(op0) comma pp(op1)
      case Xor(op0, op1)           => "xor" <+> ppTyped(op0) comma pp(op1)
      case SDiv(exact, op0, op1)   => "sdiv" <+> ppTyped(op0) comma pp(op1)
      case UDiv(exact, op0, op1)   => "udiv" <+> ppTyped(op0) comma pp(op1)
      case SRem(op0, op1)          => "srem" <+> ppTyped(op0) comma pp(op1)
      case URem(op0, op1)          => "urem" <+> ppTyped(op0) comma pp(op1)

      case FAdd(op0, op1) => "fadd" <+> ppTyped(op0) comma pp(op1)
      case FSub(op0, op1) => "fsub" <+> ppTyped(op0) comma pp(op1)
      case FMul(op0, op1) => "fmul" <+> ppTyped(op0) comma pp(op1)
      case FDiv(op0, op1) => "fdiv" <+> ppTyped(op0) comma pp(op1)
      case FRem(op0, op1) => "frem" <+> ppTyped(op0) comma pp(op1)

      case FCmp(pred, op0, op1) =>
        "fcmp" <+> pp(pred) <+> ppTyped(op0) comma pp(op1)
      case ICmp(pred, op0, op1) =>
        "icmp" <+> pp(pred) <+> ppTyped(op0) comma pp(op1)

      case c: Select =>
        "select" <+> commas(
          ppTyped(c.condition),
          ppTyped(c.trueValue),
          ppTyped(c.falseValue)
        )
      case c: SExt    => "sext" <+> ppTyped(c.operand0) <+> "to" <+> pp(c.t)
      case c: ZExt    => "zext" <+> ppTyped(c.operand0) <+> "to" <+> pp(c.t)
      case c: FPExt   => "fpext" <+> ppTyped(c.operand0) <+> "to" <+> pp(c.t)
      case c: Trunc   => "trunc" <+> ppTyped(c.operand0) <+> "to" <+> pp(c.t)
      case c: FPTrunc => "fptrunc" <+> ppTyped(c.operand0) <+> "to" <+> pp(c.t)

      case c: FPToUI => "fptoui" <+> ppTyped(c.operand0) <+> "to" <+> pp(c.t)
      case c: FPToSI => "fptosi" <+> ppTyped(c.operand0) <+> "to" <+> pp(c.t)
      case c: UIToFP => "uitofp" <+> ppTyped(c.operand0) <+> "to" <+> pp(c.t)
      case c: SIToFP => "sitofp" <+> ppTyped(c.operand0) <+> "to" <+> pp(c.t)

      case c: Struct =>
        val struct = spacedBraces(commas(c.memberValues.map(ppTyped)))
        if (c.isPacked) angleBrackets(struct) else struct

      case Null(t)          => ppNullInitializer(t)
      case AggregateZero(t) => "zeroinitializer"

      case Undef(_)  => "undef"
      case TokenNone => "none"
      case BlockAddress(fn, blk) =>
        "blockaddress" <> parens(commas(List(fn, blk).map(pp)))

      case Array(t, values) =>
        t match {
          case IntegerType(8) =>
            "c" <> dquotes(values.map {
              case Integral(8, char) => ppIntAsChar(char)
            }.mkString)
          case _ => brackets(commas(values.map(ppTyped)))
        }

      case c: GetElementPtr =>
        "getelementptr" <+> ppIf(c.inBounds, "inbounds") <+> parens(
          commas(
            pp(typeOf(c.address)) :: (c.address :: c.indices).map(ppTyped)
          )
        )

      case c: BitCast =>
        "bitcast" <+> parens(ppTyped(c.operand0) <+> "to" <+> pp(c.t))
      case c: PtrToInt =>
        "ptrtoint" <+> parens(ppTyped(c.operand0) <+> "to" <+> pp(c.t))
      case c: IntToPtr =>
        "inttoptr" <+> parens(ppTyped(c.operand0) <+> "to" <+> pp(c.t))
      case c: AddrSpaceCast =>
        "addrspacecast" <+> parens(ppTyped(c.operand0) <+> "to" <+> pp(c.t))
      case _ => throw new Exception("Non-function argument. (Malformed AST)")
    }
  }

  def pp(n: Named[String]): String = n match {
    case n := s => "%" <> pp(n) <+> "=" <+> s
    case Do(s)  => s
  }

  def pp(m: Module): String =
    "; ModuleID =" <+> singleQuotes(m.moduleName) </>
      (m.moduleDataLayout match {
        case None         => ""
        case Some(layout) => "target datalayout =" <+> dquotes(pp(layout))
      }) </>
      (m.moduleTargetTriple match {
        case None         => ""
        case Some(target) => "target triple =" <+> dquotes(target)
      }) </>
      lines(m.moduleDefinitions.map(pp))

  def pp(fp: FloatingPointPredicate): String = {
    import FloatingPointPredicate._
    fp match {
      case False => "false"
      case OEQ   => "oeq"
      case OGT   => "ogt"
      case OGE   => "oge"
      case OLT   => "olt"
      case OLE   => "ole"
      case ONE   => "one"
      case ORD   => "ord"
      case UEQ   => "ueq"
      case UGT   => "ugt"
      case UGE   => "uge"
      case ULT   => "ult"
      case ULE   => "ule"
      case UNE   => "une"
      case UNO   => "uno"
      case True  => "true"
    }
  }

  def pp(ip: IntegerPredicate): String = {
    import IntegerPredicate._
    ip match {
      case EQ  => "eq"
      case NE  => "ne"
      case UGT => "ugt"
      case UGE => "uge"
      case ULT => "ult"
      case ULE => "ule"
      case SGT => "sgt"
      case SGE => "sge"
      case SLT => "slt"
      case SLE => "sle"
    }
  }

  def pp(a: Atomicity): String = pp(a.scope) <+> pp(a.order)

  def pp(s: SynchronizationScope): String = s match {
    case SingleThread => "syncscope" <> parens(dquotes("singlethread"))
    case System       => ""
  }

  def pp(mo: MemoryOrdering): String = mo match {
    case Unordered              => "unordered"
    case Monotonic              => "monotonic"
    case Acquire                => "acquire"
    case Release                => "release"
    case AcquireRelease         => "acq_rel"
    case SequentiallyConsistent => "seq_cst"
  }

  def pp(op: RMWOperation): String = {
    import RMWOperation._
    op match {
      case Xchg => "xchg"
      case Add  => "add"
      case Sub  => "sub"
      case And  => "and"
      case Nand => "nand"
      case Or   => "or"
      case Xor  => "xor"
      case Max  => "max"
      case Min  => "min"
      case UMax => "umax"
      case UMin => "umin"
    }
  }

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

  def ppIntAsChar(i: BigInt): String = escape(i.toChar)

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

  def ppNullInitializer(t: Type): String = t match {
    case _: PointerType   => "zeroinitializer"
    case _: StructureType => "zeroinitializer"
    case _: FunctionType  => "zeroinitializer"
    case _: ArrayType     => "zeroinitializer"
    case _                => throw new Exception("Non-pointer argument. (Malformed AST)")
  }

  def ppCall(c: Instruction.Call): String = ???
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
