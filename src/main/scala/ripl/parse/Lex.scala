package ripl.parse

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

  def apply(input: String): List[Token] = Lex(input.toList)
  def apply(input: List[Char]): List[Token] = {
    val (tokens: List[Token], indentLevels: List[Int], remainingInput) =
      lexLeadingWhitespace(0, Nil, input)
    lex(tokens, indentLevels, remainingInput)
  }

  sealed trait Error
  case object Error {
    case class UnexpectedChar(c: Char)
    case class TooManyDecimalPoints(c: Char)
  }

  @tailrec
  // Must accumulate manually because scala doesn't support tail rec modulo cons
  private def lex(
      accum: List[Token],
      indentLevels: List[Int],
      input: List[Char]
    ): List[Token] = {

    import Token._

    // Could pattern match input first if some need lookahead
    input match {
      case Nil => accum.reverse ++ emitDedentsAtEnd(indentLevels)

      case ' ' :: rest => lex(accum, indentLevels, rest)
      case '(' :: rest => lex(LParen :: accum, indentLevels, rest)
      case ')' :: rest => lex(RParen :: accum, indentLevels, rest)

      case '\'' :: rest => lex(Apostrophe :: accum, indentLevels, rest)
      case '^' :: rest  => lex(Circumflex :: accum, indentLevels, rest)
      case '~' :: rest  => lex(Tilda :: accum, indentLevels, rest)

      case ';' :: rest => lex(accum, indentLevels, lexComment(rest))

      case '\n' :: rest =>
        val (tokens, newIndentLevels, remainingInput) =
          lexLeadingWhitespace(0, indentLevels, rest)
        lex(tokens ++ (Newline :: accum), newIndentLevels, rest)

      case c :: rest if c.isDigit || c == '-' =>
        val state              = if (c == '-') LexNegative else LexInt
        val (remaining, token) = lexNumberOrSymbol(List(c), state, rest)
        lex(token :: accum, indentLevels, remaining)

      case c :: rest if isValidFirstInSymbol(c) =>
        val (remaining, token) = lexNumberOrSymbol(List(c), LexSymbol, rest)
        lex(token :: accum, indentLevels, remaining)

      case '"' :: rest =>
        val (remaining, token) = lexString(Nil, rest)
        lex(token :: accum, indentLevels, remaining)
    }
  }

  sealed trait SymbolLexState
  case object LexNegative extends SymbolLexState
  case object LexInt      extends SymbolLexState
  case object LexFloat    extends SymbolLexState
  case object LexSymbol   extends SymbolLexState
  private def emitDedentsAtEnd(indentLevels: List[Int]): List[Token] =
    List.fill(indentLevels.length)(Token.Dedent)

  @tailrec
  private def lexLeadingWhitespace(
      spaceCount: Int,
      indentLevels: List[Int],
      input: List[Char]
    ): (List[Token], List[Int], List[Char]) = {

    def maxIndent(indents: List[Int]): Int = indents match {
      case Nil       => 0
      case x :: rest => x
    }

    def resolveIndentation(
        spaceCount: Int,
        indentLevels: List[Int]
      ): (List[Token], List[Int]) = {
      val maxExistingIndent = maxIndent(indentLevels)
      if (spaceCount == maxExistingIndent) {
        (Nil, indentLevels) // no change
      } else if (spaceCount > maxExistingIndent) {
        (Token.Indent :: Nil, spaceCount :: indentLevels)
      } else {

        val (levelsDedented, levelsRemaining) =
          indentLevels.span(_ > spaceCount)
        (List.fill(levelsDedented.length)(Token.Dedent), levelsRemaining)
      }
    }

    input match {

      case Nil =>
        (emitDedentsAtEnd(indentLevels), Nil, Nil)

      case c :: rest =>
        c match {
          case ' ' => lexLeadingWhitespace(spaceCount + 1, indentLevels, rest)

          case '\n' | ';' => (Nil, indentLevels, input)

          case _ =>
            val (tokens, newIndentLevels) =
              resolveIndentation(spaceCount, indentLevels)
            (tokens, newIndentLevels, input)
        }
    }
  }

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
        case LexInt                  => VInt(contents.toInt)
        case LexFloat                => VFlt(contents.toFloat)
        case LexSymbol | LexNegative => Name(contents)
      }
    }

    input match {
      // Digits don't change the state; they are valid in numbers and symbols
      case c :: rest if c.isDigit =>
        val newState = if (state == LexNegative) LexInt else state
        lexNumberOrSymbol(c :: accum, newState, rest)

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
