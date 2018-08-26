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
        _     <- i.ret(retOp)
        _     <- m.emitBlockStart(l.Name(""))
      } yield ()
    }

  def genExp(exp: Exp): IRBuilder[l.Operand] = exp match {

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

            write <- i.store(fieldAddress, 0, op)

          } yield (write)
        }
        structValue <- i.load(struct, 0)
      } yield (structValue)

    case App(fun, args) =>
      for {
        ops <- args.traverse { e: Exp =>
          genExp(e)
        }

        result <- (fun, ops) match {
          case (Intrinsic.IAdd, List(a, b)) => i.add(a, b)
          case (Intrinsic.ISub, List(a, b)) => i.sub(a, b)
          case (Intrinsic.IMul, List(a, b)) => i.mul(a, b)
          case (Intrinsic.IDiv, List(a, b)) => i.sdiv(a, b)
          case (Intrinsic.IMod, List(a, b)) => i.srem(a, b)

          case (Intrinsic.IEql, List(a, b)) =>
            i.icmp(l.IntegerPredicate.EQ, a, b)
          case (Intrinsic.ILeq, List(a, b)) =>
            i.icmp(l.IntegerPredicate.SLT, a, b)

          case (Intrinsic.And, List(a, b)) => i.and(a, b)
          case (Intrinsic.Or, List(a, b))  => i.or(a, b)

          case (Intrinsic.Truncate, List(a)) => i.trunc(a, l.IntegerType(8))

          case _ =>
            for {
              f   <- genExp(fun)
              app <- i.call(f, ops.map(l.Argument(_)))
            } yield (app)
        }
      } yield (result)

    case If(_a, _b, _c) =>
      for {
        branchName <- m.fresh()
        a          <- genExp(_a)

        ifThen = l.Name(branchName.s + ".then")
        ifElse = l.Name(branchName.s + ".else")
        ifExit = l.Name(branchName.s + ".exit")

        _ <- i.condBr(a, ifThen, ifElse)

        _         <- m.emitBlockStart(ifThen)
        b         <- genExp(_b)
        _         <- i.br(ifExit)
        ifThenEnd <- m.getCurrentBlockName()

        _         <- m.emitBlockStart(ifElse)
        c         <- genExp(_c)
        _         <- i.br(ifExit)
        ifElseEnd <- m.getCurrentBlockName()

        _      <- m.emitBlockStart(ifExit)
        result <- i.phi(List(b -> ifThenEnd, c -> ifElseEnd))

      } yield (result)

    case r.Name(nm, exp :: Nil) =>
      for {
        locals <- State.inspect { s: m.IRBuilderState =>
          s.bindings
        }
      } yield
        (locals.get(nm) match {
          case Some(op) => op
          case None =>
            val tp = exp.t
            l.ConstantOperand(
              l.Constant.GlobalReference(genType(tp), l.Name(nm))
            )
        })

    case Select(_exp, name, typ) =>
      for {
        aggregate <- genExp(_exp)
        fieldValue <- _exp.t match {
          case struct: Struct =>
            i.extractValue(aggregate, indexOfField(struct, name) :: Nil)
        }

        // Some thoughts:
        // Value to value => extract value
        // Reference to reference => gep
        // Value to reference => extract value -> alloca -> store -> gep
        // Reference to value => gep -> load

      } yield (fieldValue)

    case VBln(b) => c.bit[IRBuilder](b)
    case VInt(i) => c.int64[IRBuilder](i)
  }

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
