package ripl.parse.recursive

import scala.annotation.tailrec

case object Lex {

  import Token._

  def isC0_ControlCharacter(c: Char) = c < '\u001f'

  def isC1_ControlCharacter(c: Char) = '\u007f' <= c && c <= '\u009f'

  def isControlCharacter(c: Char) =
    isC0_ControlCharacter(c) || isC1_ControlCharacter(c)

  def isValidInSymbol(c: Char) = c match {

    // Non-control whitespace
    case ' ' => false

    // Punctuation
    // case ':' => false // May be required in future
    case '"' => false
    case '`' => false
    case ',' => false
    case '.' => false

    // Braces
    case '(' => false
    case ')' => false
    case '[' => false
    case ']' => false
    case '{' => false
    case '}' => false

    case c => !isControlCharacter(c)
  }

  def isValidFirstInSymbol(c: Char) = c match {

    // Reserved unary prefixes corresponding to the PrefixOperator tokens
    // They cannot be used at the start of symbol, though they can be used within
    case '\'' => true
    case '^' => true
    case '~' => true

    case c => isValidInSymbol(c)
  }

  def apply(input: List[Char]): List[Token] = Token.Newline :: lex(Nil, input)

  sealed trait Error
  case object Error {
    case class UnexpectedChar(c: Char)
    case class TooManyDecimalPoints(c: Char)
  }


  @tailrec
  private def lex(accum: List[Token], input: List[Char]): List[Token] = {

    // I wish something like this would work
    // @inline
    // def ret(t: Token, rest: List[Char]) = lex(Newline :: accum, rest)

    input match {
      case Nil => accum.reverse

      case '\n' :: rest => lex(Newline :: accum, rest)
      case '(' :: rest  => lex(LParen :: accum, rest)
      case ')' :: rest  => lex(RParen :: accum, rest)

      case '\'' :: rest => lex(Apostrophe :: accum, rest)
      case '^' :: rest  => lex(Circumflex :: accum, rest)
      case '~' :: rest  => lex(Tilda :: accum, rest)

      case c :: rest if c.isDigit => {
        val (remainder, tokenOrError) = lexNumber(rest)
        ???
        // lexNumber(c :: rest) match {


      }

      // case 't' :: 'r' :: 'u' :: 'e' :: rest =>
      //   rest match {
      //     case Nil => 
      //   }
    }
  }

  private def lexNumber(input: List[Char]): (List[Char], Either[Token, Error]) =
    input match {
      case c :: rest if c.isDigit => ???
    }

  // def app()
}
