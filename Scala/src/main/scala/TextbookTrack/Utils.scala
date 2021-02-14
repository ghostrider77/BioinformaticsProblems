package TextbookTrack

object Utils {
  def readGeneticCode(): Map[String, String] = {
    val lines: Iterator[String] = scala.io.Source.fromResource("RNA_codon_table.txt").getLines()
    lines.map(_.split(" ").toList).collect{ case List(codon, aminoAcid) => (codon, aminoAcid) }.toMap
  }
}
