package ripl.parse.recursive

import scala.annotation.tailrec

import ripl.ast.common._
import ripl.ast.untyped._

case object Parse {

  def apply(input: String): List[Exp]      = Parse(Lex(input))
  def apply(input: List[Token]): List[Exp] = parseTopLevelExps(Nil, input)

  @tailrec
  private def parseTopLevelExps(
      accum: List[Exp],
      input: List[Token]
    ): List[Exp] = input match {

    case Token.Newline :: rest => parseTopLevelExps(accum, rest)

    case Nil => accum.reverse

    case (atom: Atom) :: rest => parseTopLevelExps(atom :: accum, rest)

    case Token.LParen :: rest =>
      val (sExp, remaining) = parseSExp(rest)
      parseTopLevelExps(sExp :: accum, remaining)

    case Token.RParen :: _ =>
      ??? // TODO: Error case, unmatched closing parenthesis

    case Token.Indent :: _ =>
      ??? // TODO: Error case, indented top level expression

    case Token.Dedent :: _ =>
      ??? // TODO: Error case, must be some kind of error in the lexer
  }

  private def parseSExp(input: List[Token]): (Exp, List[Token]) = {
    val (contents, remaining) = parseSExpContents(Nil, input)
    (SExp(contents), remaining)
  }

  @tailrec
  private def parseSExpContents(
      accum: List[Exp],
      input: List[Token]
    ): (List[Exp], List[Token]) = input match {

    case (atom: Atom) :: rest => parseSExpContents(atom :: accum, rest)

    case Token.LParen :: rest =>
      val (sExp, remaining) = parseSExp(rest)
      parseSExpContents(sExp :: accum, remaining)

    case Token.RParen :: rest =>
      (accum.reverse, rest)
  }
}
