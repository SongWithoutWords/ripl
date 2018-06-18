// <http://llvm.org/docs/LangRef.html#data-layout>
package ripl.llvm.ast

// Little Endian is the one true way :-). Sadly, we must support the infidels.
sealed trait Endianness
case object Endianness {
  case object LittleEndian extends Endianness
  case object BigEndian    extends Endianness
}

// An AlignmentInfo(describes, how) a given type must and would best be aligned
case class AlignmentInfo(
    abiAlignment: Int,
    preferredAlignment: Int
)

// A type of type for which 'AlignmentInfo' may be specified
sealed trait AlignType
case object AlignType {
  case object Integer extends AlignType
  case object Vector  extends AlignType
  case object Float   extends AlignType
}

// A style of name mangling
sealed trait Mangling
case object Mangling {
  case object ELFMangling         extends Mangling
  case object MIPSMangling        extends Mangling
  case object MachOMangling       extends Mangling
  case object WindowsCOFFMangling extends Mangling
}

// a description of the various data layout properties which may be used during
// optimization
case class DataLayout(
    endianness: Endianness,
    mangling: Option[Mangling],
    stackAlignment: Option[Int],
    pointerLayouts: Map[AddrSpace, (Int, AlignmentInfo)],
    typeLayouts: Map[(AlignType, Int), AlignmentInfo],
    aggregateLayout: AlignmentInfo,
    nativeSizes: Option[Set[Int]]
)

case object DataLayout {
  def default(endianness: Endianness) = DataLayout(
    endianness,
    None,
    None,
    Map(
      (AddrSpace(0), (64, AlignmentInfo(64, 64)))
    ),
    Map(
      ((AlignType.Integer, 1), AlignmentInfo(8, 8)),
      ((AlignType.Integer, 8), AlignmentInfo(8, 8)),
      ((AlignType.Integer, 16), AlignmentInfo(16, 16)),
      ((AlignType.Integer, 32), AlignmentInfo(32, 32)),
      ((AlignType.Integer, 64), AlignmentInfo(32, 64)),
      ((AlignType.Float, 16), AlignmentInfo(16, 16)),
      ((AlignType.Float, 32), AlignmentInfo(32, 32)),
      ((AlignType.Float, 64), AlignmentInfo(64, 64)),
      ((AlignType.Float, 128), AlignmentInfo(128, 128)),
      ((AlignType.Vector, 64), AlignmentInfo(64, 64)),
      ((AlignType.Vector, 128), AlignmentInfo(128, 128))
    ),
    AlignmentInfo(0, 64),
    None
  )
}
