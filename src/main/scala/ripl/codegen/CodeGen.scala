package ripl.codegen

import cats._
import cats.data.State
import cats.syntax.MonadOps
import cats.instances._
import cats.implicits._

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

  def genUnit(name: String, node: Node): l.Definition = l.GlobalDefinition(
    node match {
      case Fun(params, rType, exp) =>
        l.Function(
          callingConvention = l.CallingConvention.Fast,
          name = l.Name(name),
          parameters = l.Parameters(params.map(genParam)),
          returnType = genType(rType),
          basicBlocks = genFun(params, exp)
        )
    }
  )

  def genParam(p: Param): l.Parameter = l.Parameter(genType(p.t), l.Name(p.n))

  import m.IRBuilderAliases._

  def genFun(params: List[Param], e: Exp): List[l.BasicBlock] =
    m.execIRBuilder {
      val name = l.Name("entry")
      for {
        _     <- m.emitBlockStart(name)
        retOp <- genExp(e)
        _     <- i.ret(retOp)
        _     <- m.emitBlockStart(l.Name(""))
      } yield ()
    }

  def genExp(exp: Exp): IRBuilder[l.Operand] = exp match {
    case App(fun, args) =>
      for {
        ops <- args.traverse { e: Exp =>
          genExp(e)
        }

        result <- (fun, ops) match {
          case (Intrinsic.IAdd, List(a, b)) =>
            i.add(a, b)

          case _ =>
            for {
              f   <- genExp(fun)
              app <- i.call(f, ops.map(l.Argument(_)))
            } yield (app)
        }
      } yield (result)

    case VInt(i) => c.int64[IRBuilder](i)
  }

  def genType(t: Type): l.Type = t match {
    case TInt => l.IntegerType(64)
  }

}
