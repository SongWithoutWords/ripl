package example

import scala.collection.mutable.Map

import ast._

case class Reduce() {

  def apply(e: Exp): Exp = e match {

    case App(Name("+"), List(VInt(a), VInt(b))) => VInt(a + b)

    case _ => e
  }
}

