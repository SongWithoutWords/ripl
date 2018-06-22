package ripl.parse.recursive

import scala.annotation.tailrec

import ripl.ast.common._
import ripl.ast.untyped._

case object Parse {

  def apply(input: String): List[Exp]      = Parse(Lex(input))
  def apply(input: List[Token]): List[Exp] = parseTopLevelExps(Nil, input)

  @tailrec
  // Must accumulate manually because scala doesn't support tail rec modulo cons
  private def parseTopLevelExps(
      accum: List[Exp],
      input: List[Token]
    ): List[Exp] = input match {

    case Nil => accum.reverse

    case Token.Newline :: rest => parseTopLevelExps(accum, rest)

    case Token.RParen :: _ =>
      ??? // TODO: Error case, unmatched closing parenthesis

    case Token.Indent :: _ =>
      ??? // TODO: Error case, indented top level expression

    case Token.Dedent :: _ =>
      ??? // TODO: Error case, must be some kind of error in the lexer

    case rest =>
      val (exp, remaining) = parseLine(rest)
      parseTopLevelExps(exp :: accum, remaining)
  }

  private def parseLine(input: List[Token]): (Exp, List[Token]) = {
    val (contents, remaining) = parseLineContents(Nil, input)
    val exp = contents match {
      case Nil      => SExp()
      case e :: Nil => e
      case es       => SExp(es)
    }
    (exp, remaining)
  }

  @tailrec
  private def parseLineContents(
      accum: List[Exp],
      input: List[Token]
    ): (List[Exp], List[Token]) = input match {

    case (atom: Atom) :: rest => parseLineContents(atom :: accum, rest)

    case Token.LParen :: rest =>
      val (sExp, remaining) = parseSExp(rest)
      parseLineContents(sExp :: accum, remaining)

    case Token.Newline :: rest =>
      (accum.reverse, rest)

    case Nil =>
      (accum.reverse, Nil)
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
