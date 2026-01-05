package calc

import parsley.Parsley
import parsley.Parsley.{eof, many}
import parsley.character.{digit, char, whitespace}
import parsley.expr.{precedence, Ops, InfixL, Prefix}
import parsley.syntax.character.charLift

object Parser {
  type Result[A] = (List[Token], Either[String, A])

  // Entry point: The outside world calls this
  def parse(tokens: List[Token]): Either[String, Expr] =
    expression(tokens) match
      case (Nil, Right(ast)) => Right(ast)
      case (rem, Right(_))   => Left(s"Unexpected tokens at end: $rem")
      case (_, Left(err))    => Left(err)

  // left accumulator pattern, we parse the first term and call the helper with the acc
  private def expression(tokens: List[Token]): Result[Expr] = term(
    tokens
  ) match {
    case (toksAfterTerm, Right(newexpr)) => expression(toksAfterTerm, newexpr)
    case (rem, Left(err))                => (rem, Left(err))
  }

  // we want to check the list of tokens for a + or -. if found we keep going.
  private def expression(tokens: List[Token], acc: Expr): Result[Expr] =
    tokens match {
      case Plus :: rest => {
        val (remaining, result) = term(rest)
        result match {
          case Right(terms) => expression(remaining, Add(acc, terms))
          case Left(err)    => (remaining, Left(err))
        }

      }
      case Minus :: rest => {
        val (remaining, result) = term(rest)
        result match {
          case Right(terms) => expression(remaining, Sub(acc, terms))
          case Left(err)    => (remaining, Left(err))
        }
      }
      case _ => (tokens, Right(acc))
    }

  // Multiplication & Division (Medium Precedence)
  private def term(tokens: List[Token]): Result[Expr] = factor(tokens) match {
    case (rem, Right(e))  => term(rem, e)
    case (rem, Left(err)) => (rem, Left(err))
  }

  private def term(tokens: List[Token], acc: Expr): Result[Expr] =
    tokens match {
      case Times :: rest =>
        val (rem, res) = factor(rest)
        res match {
          case Right(f) => term(rem, Mul(acc, f))
          case Left(e)  => (rem, Left(e))
        }
      case Divide :: rest =>
        val (rem, res) = factor(rest)
        res match {
          case Right(f) => term(rem, Div(acc, f))
          case Left(e)  => (rem, Left(e))
        }
      case _ => (tokens, Right(acc))
    }

  //  Numbers, Unary Minus, and Parentheses (Highest Precedence)
  private def factor(tokens: List[Token]): Result[Expr] = tokens match {
    case Num(n) :: rest =>
      (rest, Right(Literal(n)))

    case Minus :: rest =>
      factor(rest) match {
        case (rem, Right(e))  => (rem, Right(Neg(e)))
        case (rem, Left(err)) => (rem, Left(err))
      }

    case LParen :: rest =>
      expression(rest) match {
        case (RParen :: afterParen, Right(e)) => (afterParen, Right(e))
        case (rem, Right(_))                  =>
          (rem, Left("Expected matching closing parenthesis ')'"))
        case (rem, Left(err)) => (rem, Left(err))
      }

    case t :: _ =>
      (tokens, Left(s"Unexpected token '$t' where a value or '(' was expected"))

    case Nil =>
      (Nil, Left("Unexpected end of input: expected a value or '('"))
  }
}

object ParsleyParser {

  private def lexeme[A](p: Parsley[A]): Parsley[A] = p <* many(whitespace)

  private val number = lexeme(
    digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)
  ).map(Literal.apply)

  private lazy val atom: Parsley[Expr] =
    lexeme(char('(')) *> expr <* lexeme(char(')')) | number

  private lazy val expr: Parsley[Expr] = precedence[Expr](atom)(
    Ops(Prefix)(lexeme(char('-')) #> Neg.apply),
    Ops(InfixL)(lexeme(char('*')) #> Mul.apply, lexeme(char('/')) #> Div.apply),
    Ops(InfixL)(lexeme(char('+')) #> Add.apply, lexeme(char('-')) #> Sub.apply)
  )

  val parser: Parsley[Expr] = many(whitespace) *> expr <* eof
}
