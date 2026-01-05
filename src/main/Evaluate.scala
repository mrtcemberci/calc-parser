package calc

import parsley.Result
import parsley.Success
import parsley.Failure

object Evaluate {

  private def evaluate(input: Expr): Int = input match {
    case Add(l, r)  => evaluate(l) + evaluate(r)
    case Div(l, r)  => evaluate(l) / evaluate(r)
    case Literal(v) => v
    case Mul(l, r)  => evaluate(l) * evaluate(r)
    case Neg(e)     => -evaluate(e)
    case Sub(l, r)  => evaluate(l) - evaluate(r)
  }

  def evaluate(input: String): Int = {
    Parser.parse(Lexer.tokenize(input)) match {
      case Left(message) => sys.error(s"Evaluation Error: $message")
      case Right(ast)    => evaluate(ast)
    }
  }

  def evaluateParsley(input: String): Int = {
    ParsleyParser.parser.parse(input) match {
      case Success(ast)   => evaluate(ast)
      case Failure(error) => sys.error(s"Parsley Evaluation Error: $error")
    }
  }
}
