package ripl.integration

import org.scalatest._

import ripl.util.{MultiMap => Multi}

import ripl.ast.common._
import ripl.ast.common.TypeAtom._
import ripl.ast.common.ImplicitConversions._
import ripl.ast.{untyped => a0, typed => a1}

import ripl.parse.Lex
import ripl.parse.Parse

import ripl.reduce.Reduce
import ripl.reduce.CustomMatchers.matchAst

class TestIntegration extends FreeSpec with Matchers {

  def test(
      in: String
    )(out: (String, a1.Node)*
    )(errs: Error*
    ): Unit = Reduce(Parse(Lex(in))) should matchAst((Multi(out: _*), Set(errs: _*)))

  def testErrs(in: Multi[String, a0.Exp])(errs: Error*): Unit =
    Reduce(in)._2.shouldBe(Set(errs: _*))

  def testErrs(in: (String, a0.Exp)*)(errs: Error*): Unit =
    testErrs(Multi(in: _*))(errs: _*)
}
