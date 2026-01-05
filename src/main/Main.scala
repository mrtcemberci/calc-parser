package calc

@main def runCalculator(expression: String): Unit = {
  try {
    val manualResult = Evaluate.evaluate(expression)
    println(s"Manual Parser Result: $manualResult")

    val parsleyResult = Evaluate.evaluateParsley(expression)
    println(s"Parsley Parser Result: $parsleyResult")

  } catch {
    case e: Exception =>
      System.err.println(s"Error: ${e.getMessage}")
      sys.exit(1)
  }
}
