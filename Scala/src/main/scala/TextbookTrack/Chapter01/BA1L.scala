package TextbookTrack.Chapter01

object BA1L {
  private val NucleotideOrder: Map[Char, Int] = Map('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)

  def patternToNumber(pattern: String): Long =
    pattern.foldLeft(0L)((acc, nucleotide) => 4 * acc + NucleotideOrder(nucleotide))

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val dna: String = reader.next()
    val result: Long = patternToNumber(dna)
    println(result)
  }
}
