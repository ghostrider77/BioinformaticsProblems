package TextbookTrack.Chapter01

object BA1D {
  def findPatternOccurrences(text: String, pattern: String): List[Int] =
    text
      .sliding(pattern.length)
      .zipWithIndex
      .filter { case (substring, _) => substring == pattern }
      .map { case (_, ix) => ix }
      .toList

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val pattern: String = reader.next()
    val genome: String = reader.next()
    val result: List[Int] = findPatternOccurrences(genome, pattern)
    println(result.mkString(" "))
  }
}
