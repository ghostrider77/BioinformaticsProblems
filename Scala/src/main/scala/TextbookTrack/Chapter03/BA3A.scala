package TextbookTrack.Chapter03

object BA3A {
  def calcKMerComposition(text: String, k: Int): List[String] = text.sliding(k).toList.sorted

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val k: Int = reader.next().toInt
    val text: String = reader.next()
    val result: List[String] = calcKMerComposition(text, k)
    result.foreach(println)
  }
}
