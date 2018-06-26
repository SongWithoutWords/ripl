package ripl.llvm.irBuilder

import cats._
import cats.data.State
import cats.syntax.MonadOps
import cats.instances._

import ripl.llvm.ast._
import ripl.llvm.irBuilder.Internal.SnocList

object IRBuilderAliases {
  type IRBuilder[a] = State[IRBuilderState, a]
}
import IRBuilderAliases._

// A partially constructed block as a sequence of instructions
case class PartialBlock(
    partialBlockName: Name,
    partialBlockInstrs: SnocList[Named[Instruction]],
    partialBlockTerm: Option[Named[Terminator]]
  )

case object PartialBlock {
  def empty(name: Name) = PartialBlock(name, SnocList(Nil), None)
}

// Builder monad state
case class IRBuilderState(
    builderSupply: Int = 0,
    // TODO: I'll need to fall back on the set idea,
    // the map idea could fail in cases where the name ends in a number
    // builderUsedNames: Set[String],
    builderUsedNames: Map[String, Int] = Map(),
    builderNameSuggestion: Option[String] = None,
    builderBlocks: SnocList[BasicBlock] = SnocList(Nil),
    builderBlock: Option[PartialBlock] = None,
    bindings: Map[String, Operand] = Map()
  )

case object runIRBuilder {
  def apply[A](
      initialState: IRBuilderState,
      computation: IRBuilder[A]
    ): (A, List[BasicBlock]) = {
    val (finalState, result) = computation.run(initialState).value
    (result, finalState.builderBlocks.getSnocList)
  }
}

case object execIRBuilder {
  def apply[A](computation: IRBuilder[A]): List[BasicBlock] =
    runIRBuilder(IRBuilderState(), computation)._2

  def apply[A](
      initialState: IRBuilderState,
      computation: IRBuilder[A]
    ): List[BasicBlock] =
    runIRBuilder(initialState, computation)._2
}

//////////////////////////////////////////////////////////////////////////////-
// * Low-level functionality
//////////////////////////////////////////////////////////////////////////////-

case object modifyBlock {
  def apply(f: (PartialBlock) => PartialBlock): IRBuilder[Unit] =
    for {
      mbb <- State.inspect((s: IRBuilderState) => s.builderBlock)
      _ <- mbb match {
        case None =>
          for {
            nm <- freshUnName()
            _ <- State.modify { (s: IRBuilderState) =>
              s.copy(builderBlock = Some(f(PartialBlock.empty(nm))))
            }
          } yield ()
        case Some(bb) =>
          for {
            _ <- State.modify { (s: IRBuilderState) =>
              s.copy(builderBlock = Some(f(bb)))
            }
          } yield ()
      }
    } yield ()
}

case object fresh {
  def apply(): IRBuilder[Name] =
    for {
      msuggestion <- State.inspect(
        (s: IRBuilderState) => s.builderNameSuggestion
      )
      name <- msuggestion match {
        case Some(suggestion) => freshName(suggestion)
        case None             => freshUnName()
      }
    } yield (name)
}

case object freshUnName {
  def apply(): IRBuilder[Name] =
    for {
      n <- State.inspect { s: IRBuilderState =>
        s.builderSupply
      }
      _ <- State.modify { s: IRBuilderState =>
        s.copy(builderSupply = s.builderSupply + 1)
      }
    } yield (Name(n.toString()))
}

case object freshName {
  def apply(hint: String): IRBuilder[Name] =
    for {
      usedNames <- State.inspect { s: IRBuilderState =>
        s.builderUsedNames
      }
      nameUsedCount = usedNames.get(hint) match {
        case None    => 0
        case Some(n) => n + 1
      }
      _ <- State.modify { (s: IRBuilderState) =>
        s.copy(builderUsedNames = usedNames + Tuple2(hint, nameUsedCount))
      }
    } yield
      (Name(hint + (if (nameUsedCount > 0) nameUsedCount.toString else "")))
}

case object getCurrentBlockName {
  def apply(): IRBuilder[Name] =
    State.inspect { s: IRBuilderState =>
      s.builderBlock.map(_.partialBlockName).getOrElse(Name(""))
    }
}

case object addBinding {
  def apply(nm: String, op: Operand): IRBuilder[Unit] = {

    State.modify { (s: IRBuilderState) =>
      s.copy(bindings = s.bindings + Tuple2(nm, op))
    }
  }
}

case object emitInstr {
  def apply(returnType: Type, instruction: Instruction): IRBuilder[Operand] =
    for {
      nm <- fresh()
      _ <- modifyBlock { p: PartialBlock =>
        p.copy(
          partialBlockInstrs = p.partialBlockInstrs.snoc(:=(nm, instruction))
        )
      }
    } yield (LocalReference(returnType, nm))
}

case object emitInstrVoid {
  def apply(instruction: Instruction): IRBuilder[Unit] =
    modifyBlock { p: PartialBlock =>
      p.copy(partialBlockInstrs = p.partialBlockInstrs.snoc(Do(instruction)))
    }
}

case object emitTerm {
  def apply(t: Terminator): IRBuilder[Unit] =
    modifyBlock { p: PartialBlock =>
      p.copy(partialBlockTerm = Some(Do(t)))
    }
}

case object emitBlockStart {
  def apply(nm: Name): IRBuilder[Unit] =
    for {
      mbb <- State.inspect { s: IRBuilderState =>
        s.builderBlock
      }
      _ <- mbb match {
        case Some(partialBlock) =>
          val instrs = partialBlock.partialBlockInstrs
          val terminator: Named[Terminator] =
            partialBlock.partialBlockTerm match {
              case None       => Do(Terminator.Ret(None, InstructionMetadata()))
              case Some(term) => term
            }
          for {
            _ <- State.modify { s: IRBuilderState =>
              s.copy(
                builderBlocks = s.builderBlocks.snoc(
                  BasicBlock(
                    partialBlock.partialBlockName,
                    instrs.getSnocList,
                    terminator
                  )
                )
              )
            }
          } yield ()
        case None => //State.modify(s: IRBuilderState => s)
          Monad[IRBuilder].pure(())
      }
      _ <- State.modify { s: IRBuilderState =>
        s.copy(builderBlock = Some(PartialBlock.empty(nm)))
      }
    } yield ()
}

//////////////////////////////////////////////////////////////////////////////-
// * High-level functionality
//////////////////////////////////////////////////////////////////////////////-

// Starts a new block and ends the previous one
case object block {
  def apply(): IRBuilder[Name] =
    for {
      nm <- fresh()
      _  <- emitBlockStart(nm)
    } yield (nm)
}

// @ir `named` name@ executes the 'IRBuilder' @ir@ using @name@ as the base
// name whenever a fresh local name is generated. Collisions are avoided by
// appending numbers (first @"name"@, then @"name1"@, @"name2"@, and so on).

case object named {
  def apply[A](ir: IRBuilder[A], name: String): IRBuilder[A] =
    for {
      before <- State.inspect { s: IRBuilderState =>
        s.builderNameSuggestion
      }
      _ <- State.modify { s: IRBuilderState =>
        s.copy(builderNameSuggestion = Some(name))
      }
      result <- ir
      _ <- State.modify { s: IRBuilderState =>
        s.copy(builderNameSuggestion = before)
      }
    } yield (result)
}
