package TextbookTrack.Chapter02

object BA2H {
  private def calcHammingDistance(s1: String, s2: String): Int = s1.lazyZip(s2).count{ case (c1, c2) => c1 != c2 }

  private def distanceBetweenPatternAndText(text: String, pattern: String, k: Int): Int =
    text.sliding(k).foldLeft(k)((acc, kMer) => math.min(acc, calcHammingDistance(pattern, kMer)))

  def distanceBetweenPatternAndCollectionOfTexts(texts: List[String], pattern: String, k: Int): Int =
    texts.foldLeft(0)((acc, text) => acc + distanceBetweenPatternAndText(text, pattern, k))

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val pattern: String = reader.next()
    val k: Int = pattern.length
    val texts: List[String] = reader.next().split(" ").toList
    val result: Int = distanceBetweenPatternAndCollectionOfTexts(texts, pattern, k)
    println(result)
  }
}
