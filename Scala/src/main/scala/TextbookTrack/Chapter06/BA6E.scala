package TextbookTrack.Chapter06

object BA6E {
  private val NucleotideComplements: Map[Char, Char] = Map('A' -> 'T', 'C' -> 'G', 'T' -> 'A', 'G' -> 'C')

  private def calcReverseComplement(dna: String): String = dna.reverseIterator.map(NucleotideComplements).mkString

  private def collectKMerIndices(genome: String, k: Int): Map[String, List[Int]] =
    genome
      .sliding(k)
      .zipWithIndex
      .toList
      .groupMap{ case (kMer, _) => kMer }{ case (_, ix) => ix }

  def calcSharedKMers(s1: String, s2: String, k: Int): List[(Int, Int)] = {
    val kMerIndices: Map[String, List[Int]] = collectKMerIndices(s1, k)
    s2
      .sliding(k)
      .zipWithIndex
      .flatMap{ case (kMer, jy) =>
        val reversedKMer: String = calcReverseComplement(kMer)
        (kMerIndices.getOrElse(kMer, Nil) ::: kMerIndices.getOrElse(reversedKMer, Nil)).map(ix => (ix, jy))
      }
      .toList
  }


  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val k: Int = reader.next().toInt
    val s1: String = reader.next()
    val s2: String = reader.next()
    val result: List[(Int, Int)] = calcSharedKMers(s1, s2, k)
    result.foreach{ case (ix, jy) => println(s"($ix, $jy)") }
  }
}
