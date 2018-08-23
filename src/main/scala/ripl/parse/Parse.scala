package ripl.parse

import scala.annotation.tailrec

import ripl.ast.common._
import ripl.ast.parse._

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

    case Token.Dot :: rest =>
      parseAtomOrSExp(rest) match {
        case (Some(exp), remaining) =>
          accum match {
            case last :: beforeLast =>
              parseLineContents(Select(last, exp) :: beforeLast, remaining)
          }
      }

    case Token.Newline :: Token.Indent :: rest =>
      val (exps, remaining) = parseIndentedContinuation(rest)
      parseLineContents(exps ++ accum, remaining)

    case Token.Newline :: rest =>
      (accum.reverse, rest)

    case Token.Dedent :: rest =>
      (accum.reverse, rest)

    case Nil =>
      (accum.reverse, Nil)
  }

  private def parseIndentedContinuation(
      input: List[Token]
    ): (List[Exp], List[Token]) = parseIndentedContents(Nil, input)

  private def parseIndentedContents(
      accum: List[Exp],
      input: List[Token]
    ): (List[Exp], List[Token]) = input match {

    // Don't reverse accum, it's going to be embedded in the containing line

    case Nil => (accum, Nil)

    case Token.Dedent :: rest =>
      (accum, Token.Dedent :: rest)

    case Token.Newline :: Token.Dedent :: rest =>
      (accum, Token.Dedent :: rest)

    case Token.Newline :: rest => parseIndentedContents(accum, rest)

    case rest =>
      val (exp, remaining) = parseLine(rest)
      parseIndentedContents(exp :: accum, remaining)
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

    case Token.Dot :: rest =>
      parseAtomOrSExp(rest) match {
        case (Some(exp), remaining) =>
          accum match {
            case last :: beforeLast =>
              parseSExpContents(Select(last, exp) :: beforeLast, remaining)
          }
      }

    case Token.RParen :: rest =>
      (accum.reverse, rest)
  }

  private def parseAtomOrSExp(input: List[Token]): (Option[Exp], List[Token]) =
    input match {
      case (atom: Atom) :: rest => (Some(atom), rest)

      case Token.LParen :: rest =>
        val (sExp, remaining) = parseSExp(rest)
        (Some(sExp), remaining)

      case _ => (None, input)
    }
}
