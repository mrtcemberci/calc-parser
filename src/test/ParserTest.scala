package calc

class ParserTest extends munit.FunSuite {

  def parse(input: String): Either[String, Expr] =
    Parser.parse(Lexer.tokenize(input))

  test("Parser handles simple addition") {
    assertEquals(
      parse("1 + 2"),
      Right(Add(Literal(1), Literal(2)))
    )
  }

  test("Parser respects precedence (multiplication binds tighter)") {
    // 1 + 2 * 3 should be 1 + (2 * 3)
    val expected = Add(Literal(1), Mul(Literal(2), Literal(3)))
    assertEquals(parse("1 + 2 * 3"), Right(expected))
  }

  test("Parser respects parentheses (overriding precedence)") {
    // (1 + 2) * 3
    val expected = Mul(Add(Literal(1), Literal(2)), Literal(3))
    assertEquals(parse("(1 + 2) * 3"), Right(expected))
  }

  test("Parser handles left-associativity (subtraction)") {
    // 10 - 5 - 2 should be (10 - 5) - 2 -> 3
    // If it were 10 - (5 - 2), it would be 7 (Wrong)
    val expected = Sub(Sub(Literal(10), Literal(5)), Literal(2))
    assertEquals(parse("10 - 5 - 2"), Right(expected))
  }

  test("Parser handles unary minus") {
    // -5 + 3
    val expected = Add(Neg(Literal(5)), Literal(3))
    assertEquals(parse("-5 + 3"), Right(expected))
  }

  test("Parser returns error on unbalanced parentheses") {
    val result = parse("(1 + 2")
    assert(result.isLeft)
  }
}
