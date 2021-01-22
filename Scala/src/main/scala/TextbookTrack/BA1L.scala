package TextbookTrack

object BA1L {
  private val NucleotideOrder: Map[Char, Int] = Map('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)

  def patternToNumber(pattern: String): Int =
    pattern.foldLeft(0)((acc, nucleotide) => 4 * acc + NucleotideOrder(nucleotide))

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val dna: String = reader.next()
    val result: Long = patternToNumber(dna)
    println(result)
  }
}
