package calc

@main def runCalculator(expression: String): Unit = {
  try {
    val result = Evaluate.evaluate(expression)
    println(s"Result: $result")
  } catch {
    case e: Exception =>
      System.err.println(s"Error: ${e.getMessage}")
      sys.exit(1)
  }
}