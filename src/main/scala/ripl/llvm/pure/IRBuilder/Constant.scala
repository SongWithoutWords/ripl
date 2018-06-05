package ripl.llvm.pure.IRBuilder

import ripl.llvm.pure.ast._

case object Constant {
  // def int64[F[_]](i: Long): F[Operand] =
  //   pure(ConstantOperand(Integral(64, i)))

  // def int32[F[_]](i: Long): F[Operand] =
  //   pure(ConstantOperand(Integral(32, i)))

  // def bit[F[_]](b: Boolean): F[Operand] =
  //   pure(ConstantOperand(Integral(1, if (b) 1 else 0)))

  // def double[F[_]](f: scala.Double): F[Operand] =
  //   pure(ConstantOperand(Float(Double(f))))

  // def single[F[_]](f: scala.Float): F[Operand] =
  //   pure(ConstantOperand(Float(Single(f))))

  // def half[f <: Applicative[_]](f: Float): f[Operand] =
  //   pure(ConstantOperand(Single(1, i)))

  // def struct[F[_] <: Applicative](
  //     name: Option[Name],
  //     packed: Boolean,
  //     members: List[Constant],
  //     pure: Pure[A]
  // ): F[Operand] =
  //   pure(ConstantOperand(Struct(name, packed, members)))

  // def struct[F[_]](members: List[Constant]): F[Operand] =
  //   pure(ConstantOperand(Array(typeOf(members.head), members)))
}
