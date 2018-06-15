// Querying the type of LLVM expressions
package ripl.llvm.pure.ast

import TypeAliases._

case object typeOf {

  def apply(op: Operand): Type = op match {
    case LocalReference(t, _) => t
    case ConstantOperand(c)   => typeOf(c)
    case _                    => MetadataType
  }

  def apply(op: CallableOperand): Type = op match {
    case op: Operand => typeOf(op)
    case _: InlineAssembly =>
      throw new Exception(
        "typeOf inline assembler is not defined (Malformed AST)"
      )
  }

  def apply(c: Constant): Type = {
    import Constant._
    c match {
      case Integral(bits, _) => IntegerType(bits)
      case Float(f)          => typeOf(f)
      case Null(t)           => t
      case Struct(_, isPacked, memberValues) =>
        StructureType(isPacked, memberValues.map(typeOf(_)))
      case Array(memberType, memberValues) =>
        ArrayType(memberValues.size, memberType)
      case Vector(memberValues) =>
        VectorType(
          memberValues.size,
          memberValues match {
            case Nil =>
              throw new Exception(
                "Vectors of size zero are not allowed. (Malformed AST)"
              )
            case (x :: _) => typeOf(x)
          }
        )

      case Undef(t)              => t
      case c: BlockAddress       => ptr(i8)
      case GlobalReference(t, _) => t
      case c: Add                => typeOf(c.operand0)
      case c: FAdd               => typeOf(c.operand0)
      case c: FDiv               => typeOf(c.operand0)
      case c: FRem               => typeOf(c.operand0)
      case c: Sub                => typeOf(c.operand0)
      case c: FSub               => typeOf(c.operand0)
      case c: Mul                => typeOf(c.operand0)
      case c: FMul               => typeOf(c.operand0)
      case c: UDiv               => typeOf(c.operand0)
      case c: SDiv               => typeOf(c.operand0)
      case c: URem               => typeOf(c.operand0)
      case c: SRem               => typeOf(c.operand0)
      case c: Shl                => typeOf(c.operand0)
      case c: LShr               => typeOf(c.operand0)
      case c: AShr               => typeOf(c.operand0)
      case c: And                => typeOf(c.operand0)
      case c: Or                 => typeOf(c.operand0)
      case c: Xor                => typeOf(c.operand0)
      case c: GetElementPtr      => getElementPtrType(typeOf(c.address), c.indices)
      case c: Trunc              => c.t
      case c: ZExt               => c.t
      case c: SExt               => c.t
      case c: FPToUI             => c.t
      case c: FPToSI             => c.t
      case c: UIToFP             => c.t
      case c: SIToFP             => c.t
      case c: FPTrunc            => c.t
      case c: FPExt              => c.t
      case c: PtrToInt           => c.t
      case c: IntToPtr           => c.t
      case c: BitCast            => c.t
      case c: ICmp =>
        typeOf(c.operand0) match {
          case VectorType(n, _) => VectorType(n, i1)
          case _                => i1
        }

      case c: FCmp =>
        typeOf(c.operand0) match {
          case VectorType(n, _) => VectorType(n, i1)
          case _                => i1
        }

      case c: Select => typeOf(c.trueValue)
      case c: ExtractElement =>
        typeOf(c.vector) match {
          case VectorType(_, t) => t
          case _ =>
            throw new Exception(
              "The first operand of an extractelement instruction is a value of vector type. (Malformed AST)"
            )
        }
      case c: InsertElement => typeOf(c.vector)
      case c: ShuffleVector =>
        (typeOf(c.operand0), typeOf(c.mask)) match {
          case (VectorType(_, t), VectorType(m, _)) => VectorType(m, t)
          case _ =>
            throw new Exception(
              "The first operand of an shufflevector instruction is a value of vector type. (Malformed AST)"
            )
        }
      case c: ExtractValue  => extractValueType(c.indices, typeOf(c.aggregate))
      case c: InsertValue   => typeOf(c.aggregate)
      case TokenNone        => TokenType
      case c: AddrSpaceCast => c.t
    }
  }

  def apply(f: SomeFloat): Type = f match {
    // case Half(_)     => HalfFP
    case Single(_) => FloatFP
    case Double(_) => DoubleFP
    // case F.Quadruple(_, _)   => FP128FP
    // case F.X86_FP80(_, _)    => X86_FP80FP
    // case F.PPC_FP128(_, _)   => PPC_FP128FP
  }

  def apply(g: Global): Type = g match {
    case g: GlobalVariable => g.t
    case g: GlobalAlias    => g.t
    case g: Function       =>
      // val (params, isVarArg: Boolean) = g.parameters
      FunctionType(
        g.returnType,
        g.parameters.params.map(typeOf(_)),
        g.parameters.isVarArg
      )
  }

  def apply(p: Parameter): Type = p.t
}

case object getElementPtrType {
  def apply(t: Type, constants: List[Constant]): Type = (t, constants) match {
    case (ty, Nil)                          => ptr(ty)
    case (PointerType(ty, _), _ :: indices) => getElementPtrType(ty, indices)
    case (
        StructureType(_, elementTypes),
        Constant.Integral(32, index) :: indices
        ) =>
      getElementPtrType(elementTypes(index.toInt), indices)
    case (VectorType(_, elementType), _ :: indices) =>
      getElementPtrType(elementType, indices)
    case (ArrayType(_, elementType), _ :: indices) =>
      getElementPtrType(elementType, indices)
    case (_, _) =>
      throw new Exception("Expecting aggregate type. (Malformed AST)")
  }
}

case object getElementType {
  def apply(t: Type) = t match {
    case PointerType(t, _) => t
    case _                 => throw new Exception("Expecting pointer type. (Malformed AST)")
  }
}

case object extractValueType {
  def apply(indices: List[Int], t: Type): Type = (indices, t) match {
    case (Nil, t) => t
    case (i :: is, ArrayType(numElements, elementType)) =>
      if (i < numElements) {
        extractValueType(is, elementType)
      } else {
        throw new Exception(
          "Expecting valid index into array type. (Malformed AST)"
        )
      }
    case (i :: is, StructureType(_, elementTypes)) =>
      if (i < elementTypes.size) {
        extractValueType(is, elementTypes(i))
      } else {
        throw new Exception(
          "Expecting valid index into structure type. (Malformed AST)"
        )
      }
    case (_, _) =>
      throw new Exception("Expecting aggregate type. (Malformed AST)")
  }
}
