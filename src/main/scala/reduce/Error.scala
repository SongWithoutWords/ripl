package reduce

sealed trait Error
case class RecursiveVariableDef() extends Error
case class UnknownName() extends Error
