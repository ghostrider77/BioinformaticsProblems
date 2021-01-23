package TextbookTrack.Chapter01

object BA1G {
  def calcHammingDistance(s1: String, s2: String): Int = s1.lazyZip(s2).count { case (c1, c2) => c1 != c2 }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val dna1: String = reader.next()
    val dna2: String = reader.next()
    val result: Int = calcHammingDistance(dna1, dna2)
    println(result)
  }
}
