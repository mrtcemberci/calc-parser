package calc

class LexerTest extends munit.FunSuite {

  test("Lexer should tokenize a simple addition") {
    val input = "1 + 2"
    val expected = List(Num(1), Plus, Num(2))
    assertEquals(Lexer.tokenize(input), expected)
  }

  test("Lexer should handle multi-digit numbers and whitespace") {
    val input = " 123   + 4567 "
    val expected = List(Num(123), Plus, Num(4567))
    assertEquals(Lexer.tokenize(input), expected)
  }

  test("Lexer should handle all operators and parentheses") {
    val input = "(10 * 2) / 5 - 3"
    val expected = List(
      LParen,
      Num(10),
      Times,
      Num(2),
      RParen,
      Divide,
      Num(5),
      Minus,
      Num(3)
    )
    assertEquals(Lexer.tokenize(input), expected)
  }

  test("Lexer should throw an exception on invalid characters") {
    val input = "1 + 2 @ 3"
    intercept[Exception] {
      Lexer.tokenize(input)
    }
  }

  test("Lexer should handle empty input") {
    assertEquals(Lexer.tokenize(""), Nil)
    assertEquals(Lexer.tokenize("   "), Nil)
  }
}
