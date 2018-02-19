package reduce

sealed trait Error
case class RecursiveVariableDef() extends Error
