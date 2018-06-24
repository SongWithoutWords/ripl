package ripl.process

import reduce._

import ripl.llvm.pretty.prettyPrint

case object GenerateLlvmIr {
  def apply(s: String): Either[String, Set[Error]] = ???
}

case object Compile {
  def apply(s: String): Option[Error] = ???
}

case object Run {

}
