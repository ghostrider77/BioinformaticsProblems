package TextbookTrack

object BA1B {
  def mostFrequentKMer(text: String, k: Int): List[String] = {
    val counts: Map[String, Int] = text.sliding(k).foldLeft(Map.empty[String, Int]){
      case (acc, k_mer) =>
        val count: Int = acc.getOrElse(k_mer, 0)
        acc.updated(k_mer, count + 1)
    }
    val maxCount: Int = counts.valuesIterator.max
    counts
      .withFilter{ case (_, count) => count == maxCount }
      .map{ case (k_mer, _) => k_mer }
      .toList
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next()
    val k: Int = reader.next().toInt
    val result: List[String] = mostFrequentKMer(text, k)
    println(result.mkString(" "))
  }
}
