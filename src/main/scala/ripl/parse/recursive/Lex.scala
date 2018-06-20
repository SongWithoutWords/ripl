package ripl.parse.recursive

import scala.annotation.tailrec

import ripl.ast.common._

case object Lex {

  def isC0_ControlCharacter(c: Char) = c < '\u001f'

  def isC1_ControlCharacter(c: Char) = '\u007f' <= c && c <= '\u009f'

  def isControlCharacter(c: Char) =
    isC0_ControlCharacter(c) || isC1_ControlCharacter(c)

  def isValidInSymbol(c: Char) = c match {

    // May require  ':' in future
    case ' ' | '"' | '`' | ',' | ';' | '.' | '(' | ')' | '[' | ']' | '{' |
        '}' =>
      false

    case c => !isControlCharacter(c)
  }

  def isValidFirstInSymbol(c: Char) = c match {

    // Reserved unary prefixes corresponding to the PrefixOperator tokens
    // They cannot be used at the start of symbol, though they can be used within
    case '\'' | '^' | '~' => true

    case c => isValidInSymbol(c)
  }

  def apply(input: String): List[Token]     = Lex(input.toList)
  def apply(input: List[Char]): List[Token] = lex(Nil, input)

  sealed trait Error
  case object Error {
    case class UnexpectedChar(c: Char)
    case class TooManyDecimalPoints(c: Char)
  }

  @tailrec
  // Must accumulate manually because scala doesn't support tail rec modulo cons
  private def lex(accum: List[Token], input: List[Char]): List[Token] = {

    import Token._

    // Could pattern match input first if some need lookahead
    input match {
      case Nil => accum.reverse

      case ' ' :: rest  => lex(accum, rest)
      case '\n' :: rest => lex(Newline :: accum, rest)
      case '(' :: rest  => lex(LParen :: accum, rest)
      case ')' :: rest  => lex(RParen :: accum, rest)

      case '\'' :: rest => lex(Apostrophe :: accum, rest)
      case '^' :: rest  => lex(Circumflex :: accum, rest)
      case '~' :: rest  => lex(Tilda :: accum, rest)

      case ';' :: rest => lex(accum, lexComment(rest))

      case c :: rest if c.isDigit =>
        val (remaining, token) = lexNumberOrSymbol(List(c), LexInt, rest)
        lex(token :: accum, remaining)

      case c :: rest if isValidFirstInSymbol(c) =>
        val (remaining, token) = lexNumberOrSymbol(List(c), LexSymbol, rest)
        lex(token :: accum, remaining)

      case '"' :: rest =>
        val (remaining, token) = lexString(Nil, rest)
        lex(token :: accum, remaining)
    }
  }

  sealed trait SymbolLexState
  case object LexInt    extends SymbolLexState
  case object LexFloat  extends SymbolLexState
  case object LexSymbol extends SymbolLexState

  @tailrec
  private def lexComment(input: List[Char]): List[Char] = input match {
    case c :: rest =>
      c match {
        case '\n' => rest
        case _    => lexComment(rest)
      }
    case Nil => Nil
  }

  // Returns the remaining input
  @tailrec
  private def lexNumberOrSymbol(
      accum: List[Char],
      state: SymbolLexState,
      input: List[Char]
    ): (List[Char], Token) = {

    // Evaluated only when the token is complete
    lazy val result = {
      val contents = accum.reverse.mkString
      state match {
        case LexInt    => VInt(contents.toInt)
        case LexFloat  => VFlt(contents.toFloat)
        case LexSymbol => Token.Symbol(contents)
      }
    }

    input match {
      // Digits don't change the state; they are valid in numbers and symbols
      case c :: rest if c.isDigit => lexNumberOrSymbol(c :: accum, state, rest)

      case '.' :: rest =>
        state match {
          case LexInt => lexNumberOrSymbol('.' :: accum, LexFloat, rest)
          case _      => (input, result)
        }

      // Characters that are valid only in symbols transition the state to Symbol
      case c :: rest if isValidInSymbol(c) =>
        lexNumberOrSymbol(c :: accum, LexSymbol, rest)

      case rest => (rest, result)
    }
  }

  @tailrec
  private def lexString(
      accum: List[Char],
      input: List[Char]
    ): (List[Char], Token) = {

    lazy val result = VStr(accum.reverse.mkString)
    input match {
      case '"' :: rest => (rest, result)
      case c :: rest   => lexString(c :: accum, rest)
      case Nil         => ??? // TODO: need a mechanism for returning lexing errors
    }
  }
}
