package calc

class EvaluateTest extends munit.FunSuite {

  test("Evaluate should compute basic addition and subtraction") {
    assertEquals(Evaluate.evaluate("10 + 5"), 15)
    assertEquals(Evaluate.evaluate("20 - 8"), 12)
  }

  test(
    "Evaluate should respect operator precedence (multiplication before addition)"
  ) {
    // 2 + 3 * 4 should be 14, not 20
    assertEquals(Evaluate.evaluate("2 + 3 * 4"), 14)
  }

  test("Evaluate should handle parentheses to override precedence") {
    // (2 + 3) * 4 should be 20
    assertEquals(Evaluate.evaluate("(2 + 3) * 4"), 20)
  }

  test("Evaluate should handle unary negation") {
    assertEquals(Evaluate.evaluate("-5 + 10"), 5)
    assertEquals(Evaluate.evaluate("--5"), 5)
  }

  test("Evaluate should handle complex nested expressions") {
    // 2 * (3 + (10 / 2)) = 2 * (3 + 5) = 16
    assertEquals(Evaluate.evaluate("2 * (3 + (10 / 2))"), 16)
  }

  test("Evaluate should throw an exception on parse errors") {
    // Testing the sys.error call for invalid syntax
    intercept[RuntimeException] {
      Evaluate.evaluate("1 + + 2")
    }
  }

  test("Evaluate should throw an exception on unknown characters") {
    // Testing the Lexer error propagation
    intercept[Exception] {
      Evaluate.evaluate("1 + $ 2")
    }
  }
}
