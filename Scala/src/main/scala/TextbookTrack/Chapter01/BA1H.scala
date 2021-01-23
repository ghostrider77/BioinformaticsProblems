package TextbookTrack.Chapter01

object BA1H {
  private def calcHammingDistance(s1: String, s2: String): Int = s1.lazyZip(s2).count { case (c1, c2) => c1 != c2 }

  def findApproximatePatternOccurrences(text: String, pattern: String, d: Int): List[Int] =
    text
      .sliding(pattern.length)
      .zipWithIndex
      .filter { case (substring, _) => calcHammingDistance(substring, pattern) <= d }
      .map { case (_, ix) => ix }
      .toList

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val pattern: String = reader.next()
    val text: String = reader.next()
    val d: Int = reader.next().toInt
    val result: List[Int] = findApproximatePatternOccurrences(text, pattern, d)
    println(result.mkString(" "))
  }
}
