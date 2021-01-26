package TextbookTrack.Chapter02

object BA2B {
  private val Nucleotides: Vector[Char] = Vector('A', 'C', 'G', 'T')

  private def calcHammingDistance(s1: String, s2: String): Int = s1.lazyZip(s2).count{ case (c1, c2) => c1 != c2 }

  private def distanceBetweenPatternAndText(text: String, pattern: String, k: Int): Int =
    text.sliding(k).foldLeft(k)((acc, kMer) => math.min(acc, calcHammingDistance(pattern, kMer)))

  private def distanceBetweenPatternAndCollectionOfTexts(texts: List[String], pattern: String, k: Int): Int =
    texts.foldLeft(0)((acc, text) => acc + distanceBetweenPatternAndText(text, pattern, k))

  private def cartesianProduct[T](xs: Seq[T], n: Int): Iterator[List[T]] =
    (0 until n).foldLeft(Iterator(List.empty[T]))((acc, _) => acc.flatMap(ys => xs.map(_ :: ys)))

  def calcMedianString(texts: List[String], k: Int): String = {
    val patterns: Iterator[String] = cartesianProduct(Nucleotides, k).map(_.mkString)
    val (_, medianString): (Int, String) = patterns.foldLeft((Int.MaxValue, "")){
      case (acc @ (minimumDistance, _), pattern) =>
        val distance: Int = distanceBetweenPatternAndCollectionOfTexts(texts, pattern, k)
        if (distance < minimumDistance) (distance, pattern) else acc
    }
    medianString
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val k: Int = reader.next().toInt
    val texts: List[String] = reader.toList
    val result: String = calcMedianString(texts, k)
    println(result)
  }
}
