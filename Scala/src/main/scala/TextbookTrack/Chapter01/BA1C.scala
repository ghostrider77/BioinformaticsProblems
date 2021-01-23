package TextbookTrack.Chapter01

object BA1C {
  private val NucleotideComplements: Map[Char, Char] = Map('A' -> 'T', 'C' -> 'G', 'T' -> 'A', 'G' -> 'C')

  def calcReverseComplement(dna: String): String = dna.reverseIterator.map(NucleotideComplements).mkString

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val dna: String = reader.next()
    val result: String = calcReverseComplement(dna)
    println(result)
  }
}
