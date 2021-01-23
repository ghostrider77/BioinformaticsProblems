package TextbookTrack.Chapter01

object BA1K {
  private val NucleotideOrder: Map[Char, Int] = Map('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)

  private def patternToNumber(pattern: String): Int =
    pattern.foldLeft(0)((acc, nucleotide) => 4 * acc + NucleotideOrder(nucleotide))

  def computingFrequencies(text: String, k: Int): Vector[Int] = {
    val frequencies: Array[Int] = Array.fill(math.pow(4, k).toInt)(0)
    val kMerEncodings: Iterator[Int] = text.sliding(k).map(patternToNumber)
    kMerEncodings.foreach(code => frequencies(code) += 1)
    frequencies.toVector
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val dna: String = reader.next()
    val k: Int = reader.next().toInt
    val result: Vector[Int] = computingFrequencies(dna, k)
    println(result.mkString(" ").take(100))
  }
}
