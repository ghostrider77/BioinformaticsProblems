package TextbookTrack

object Utils {
  val GeneticCode: Map[String, Char] = readGeneticCode()

  val IntegerMassTable: Map[Char, Int] = readAminoAcidMassTable()

  private def readGeneticCode(): Map[String, Char] = {
    val lines: Iterator[String] = scala.io.Source.fromResource("RNA_codon_table.txt").getLines()
    lines.map(_.split(" ").toList).collect{ case List(codon, aminoAcid) => (codon, aminoAcid.head) }.toMap
  }

  private def readAminoAcidMassTable(): Map[Char, Int] = {
    val lines: Iterator[String] = scala.io.Source.fromResource("integer_mass_table.txt").getLines()
    lines.map(_.split(" ").toList).collect{ case List(aminoAcid, mass) => (aminoAcid.head, mass.toInt) }.toMap
  }
}
