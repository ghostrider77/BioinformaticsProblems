package AlgorithmicHeights

object FIBO {
  def fibonacci(n: Int): Int =
    (0 until n).foldLeft((0, 1)){ case ((a, b), _) => (b, a + b) }._1

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val result: Int = fibonacci(n)
    println(result)
  }
}
