package ripl.reduce2.ast.untyped

import scala.collection.mutable.ArrayBuffer

import ripl.util.MultiMap
import ripl.ast.parse._
import ripl.ast.common._

// Unique identifier of a definition within the program
case class ID(value: Int)

case class Definitions(contents: ArrayBuffer[Exp]) {
  def apply(id: ID): Exp = contents(id.value)

  def appendNew(a: Exp): ID = {
    val count = contents.length
    contents.append(a)
    ID(count)
  }
}

case class Scopes(contents: ArrayBuffer[Scope]) {
  def apply(id: ID): Scope = contents(id.value)
}

case object Scope {
  type Contents = MultiMap[Name, Either[ID, Scope]]
}

sealed trait Scope {
  def lookup(n: Name): List[Either[ID, Scope]]
}

case class Global(contents: Scope.Contents) extends Scope {
  def lookup(n: Name) = contents(n)
}

case class Local(contents: Scope.Contents, parent: Scope) extends Scope {
  def lookup(n: Name) = contents(n) ++ parent.lookup(n)
}

case class Ast(definitions: Definitions, scopes: Scopes)
