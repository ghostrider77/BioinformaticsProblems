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

  def readScoringMatrix(name: String): Map[(Char, Char), Int] = {
    val lines: Iterator[String] = scala.io.Source.fromResource(s"$name.txt").getLines()
    val aminoAcids: List[Char] = lines.next().split("\\s+").filterNot(_ == "").map(_.head).toList
    lines.flatMap{ line =>
      val split: List[String] = line.split("\\s+").toList
      val aminoAcid1: Char = split.head.head
      split.tail.map(_.toInt).zip(aminoAcids).flatMap{
        case (score, aminoAcid2) => List(((aminoAcid1, aminoAcid2), score), ((aminoAcid2, aminoAcid1), score))
      }
    }.toMap
  }
}
