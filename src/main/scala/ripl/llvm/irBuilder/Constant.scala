package ripl.llvm.irBuilder

import ripl.llvm.ast._

import cats._
import cats.implicits._

case object Constant {
  import ripl.llvm.ast.Constant._

  def int64[F[_]: Applicative](i: Long): F[Operand] =
    Applicative[F].pure(ConstantOperand(Integral(64, BigInt(i))))

  def int32[F[_]: Applicative](i: Long): F[Operand] =
    Applicative[F].pure(ConstantOperand(Integral(32, i)))

  def bit[F[_]: Applicative](b: Boolean): F[Operand] =
    Applicative[F].pure(ConstantOperand(Integral(1, if (b) 1 else 0)))

  def double[F[_]: Applicative](f: Double): F[Operand] =
    Applicative[F].pure(ConstantOperand(F64(f)))

  def single[F[_]: Applicative](f: Float): F[Operand] =
    Applicative[F].pure(ConstantOperand(F32(f)))

  // def half[F[_]: Applicative](f: Short): F[Operand] =
  //   Applicative[F].pure(ConstantOperand(Single(1, i)))

  def struct[F[_]: Applicative](
      name: Option[Name],
      packed: Boolean,
      members: List[Constant]
  ): F[Operand] =
    Applicative[F].pure(ConstantOperand(Struct(name, packed, members)))

  def struct[F[_]: Applicative](members: List[Constant]): F[Operand] =
    Applicative[F].pure(ConstantOperand(Array(typeOf(members.head), members)))
}
