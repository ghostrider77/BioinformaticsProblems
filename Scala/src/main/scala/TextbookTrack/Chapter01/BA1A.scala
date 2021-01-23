package TextbookTrack.Chapter01

object BA1A {
  def countPattern(text: String, pattern: String): Int = text.sliding(pattern.length).count(_ == pattern)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next()
    val pattern: String = reader.next()
    val result: Int = countPattern(text, pattern)
    println(result)
  }
}
