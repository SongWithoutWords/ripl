package ripl.codegen

import cats._
import cats.data.State
import cats.syntax.MonadOps
import cats.instances._
import cats.implicits._

import ripl.ast.{typed => r}
import ripl.ast.typed._
import ripl.ast.common._
import ripl.ast.common.TypeAtom._

import ripl.llvm.{ast => l, irBuilder => m}
import ripl.llvm.irBuilder.{IRBuilderInstruction => i}
import ripl.llvm.irBuilder.{Constant => c}

case object CodeGen {
  def apply(ast: Ast): l.Module = genModule(ast)

  def genModule(ast: Ast): l.Module = {
    val definitions = ast.toList.map {
      case (name, node) => genUnit(name, node)
    }
    l.Module("ripl", "", None, None, definitions)
  }

  def genUnit(name: String, node: Node): l.Definition =
    node match {
      case External(params, rType) =>
        l.GlobalDefinition(
          l.Function(
            callingConvention = l.CallingConvention.C,
            name = l.Name(name),
            parameters = l.Parameters(params.map(genParam)),
            returnType = genType(rType),
            basicBlocks = Nil
          )
        )
      case Fun(params, rType, exp) =>
        l.GlobalDefinition(
          l.Function(
            callingConvention = l.CallingConvention.Fast,
            name = l.Name(name),
            parameters = l.Parameters(params.map(genParam)),
            returnType = genType(rType),
            basicBlocks = genFun(params, exp)
          )
        )
      case Struct(name, fields) =>
        l.TypeDefinition(l.Name(name), Some(l.StructureType(false, fields.map {
          case (_, tp) => genType(tp)
        })))
    }

  def genParam(p: Param): l.Parameter = l.Parameter(genType(p.t), l.Name(p.n))

  import m.IRBuilderAliases._

  def genFun(params: List[Param], e: Exp): List[l.BasicBlock] =
    m.execIRBuilder {
      val name = l.Name("entry")
      for {
        _ <- params.traverse { p: Param =>
          m.addBinding(p.n, l.LocalReference(genType(p.t), l.Name(p.n)))
        }
        _     <- m.emitBlockStart(name)
        retOp <- genExp(e)
        _ <- retOp match {
          case Some(op) => i.ret(op)
          case None     => i.retVoid
        }
        _ <- m.emitBlockStart(l.Name(""))
      } yield ()
    }

  def genExp(exp: Exp): IRBuilder[Option[l.Operand]] = exp match {

    case App(Constructor(struct), _args) =>
      for {
        struct <- i.alloca(genType(struct), None, 0)
        _ <- _args.traverseWithIndexM { (e: Exp, index: Int) =>
          for {
            op <- genExp(e)

            fieldAddress <- i
              .gep(
                struct,
                List(
                  l.ConstantOperand(l.Constant.Integral(64, 0)),
                  l.ConstantOperand(l.Constant.Integral(32, index))
                )
              )

            write <- i.store(fieldAddress, 0, op.get)

          } yield (write)
        }
        structValue <- i.load(struct, 0)
      } yield (Some(structValue))

    case App(fun, args) =>
      for {
        ops <- args.traverse { e: Exp =>
          genExp(e)
        }

        result <- (fun, ops) match {
          case (Intrinsic.IAdd, List(Some(a), Some(b))) => i.add(a, b)
          case (Intrinsic.ISub, List(Some(a), Some(b))) => i.sub(a, b)
          case (Intrinsic.IMul, List(Some(a), Some(b))) => i.mul(a, b)
          case (Intrinsic.IDiv, List(Some(a), Some(b))) => i.sdiv(a, b)
          case (Intrinsic.IMod, List(Some(a), Some(b))) => i.srem(a, b)

          case (Intrinsic.IEql, List(Some(a), Some(b))) =>
            i.icmp(l.IntegerPredicate.EQ, a, b)
          case (Intrinsic.ILeq, List(Some(a), Some(b))) =>
            i.icmp(l.IntegerPredicate.SLT, a, b)
          case (Intrinsic.IGeq, List(Some(a), Some(b))) =>
            i.icmp(l.IntegerPredicate.SGT, a, b)

          case (Intrinsic.And, List(Some(a), Some(b))) => i.and(a, b)
          case (Intrinsic.Or, List(Some(a), Some(b)))  => i.or(a, b)

          case (Intrinsic.Truncate, List(Some(a))) =>
            i.trunc(a, l.IntegerType(8))

          case _ =>
            for {
              f   <- genExp(fun)
              app <- i.call(f.get, ops.map(op => l.Argument(op.get)))
            } yield (app)
        }
      } yield
        (l.typeOf(result) match {
          case l.VoidType => None
          case _          => Some(result)
        })

    case Block(_exps) =>
      for {
        exps <- _exps.traverse(genExp(_))
      } yield (exps.head)

    case If(_a, _b, _c) =>
      for {
        branchName <- m.fresh()
        a          <- genExp(_a)

        ifThen = l.Name(branchName.s + ".then")
        ifElse = l.Name(branchName.s + ".else")
        ifExit = l.Name(branchName.s + ".exit")

        _ <- i.condBr(a.get, ifThen, ifElse)

        _         <- m.emitBlockStart(ifThen)
        b         <- genExp(_b)
        _         <- i.br(ifExit)
        ifThenEnd <- m.getCurrentBlockName()

        _         <- m.emitBlockStart(ifElse)
        c         <- genExp(_c)
        _         <- i.br(ifExit)
        ifElseEnd <- m.getCurrentBlockName()

        _ <- m.emitBlockStart(ifExit)
        result <- (b, c) match {
          case (Some(opb), Some(opc)) =>
            i.phi(List(opb -> ifThenEnd, opc -> ifElseEnd)).map(Some(_))
          case _ => ret(None)
        }

      } yield (result)

    case r.Name(nm, exp :: Nil) =>
      for {
        locals <- State.inspect { s: m.IRBuilderState =>
          s.bindings
        }
      } yield
        (locals.get(nm) match {
          case Some(op) => Some(op)
          case None =>
            Some(
              l.ConstantOperand(
                l.Constant.GlobalReference(genType(exp.t), l.Name(nm))
              )
            )
        })

    case Select(_exp, name, typ) =>
      for {
        aggregate <- genExp(_exp)
        fieldValue <- _exp.t match {
          case struct: Struct =>
            i.extractValue(aggregate.get, indexOfField(struct, name) :: Nil)
        }

        // Some thoughts:
        // Value to value => extract value
        // Reference to reference => gep
        // Value to reference => extract value -> alloca -> store -> gep
        // Reference to value => gep -> load

      } yield (Some(fieldValue))

    case VBln(b) =>
      ret(Some(l.ConstantOperand(l.Constant.Integral(1, if (b) 1 else 0))))
    case VInt(i) =>
      ret(Some(l.ConstantOperand(l.Constant.Integral(64, i))))
    case VNone => ret(None)
  }

  def ret[A](a: A): IRBuilder[A] = Applicative[IRBuilder].pure(a)

  def genType(t: Type): l.Type = t match {

    case TBln  => l.IntegerType(1)
    case TInt  => l.IntegerType(64)
    case TI8   => l.IntegerType(8)
    case TNone => l.VoidType

    case TFun(params, ret) => l.FunctionType(genType(ret), params.map(genType))

    case Struct(_, fields) =>
      l.StructureType(false, fields.map {
        case (_: String, t: Type) => genType(t)
      })
  }

  def indexOfField(t: Struct, name: String) = t.fields.indexWhere {
    _0: (String, Type) =>
      _0._1 == name
  }

}
