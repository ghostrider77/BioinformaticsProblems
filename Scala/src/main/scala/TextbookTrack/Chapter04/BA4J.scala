package TextbookTrack.Chapter04

object BA4J {
  import TextbookTrack.Utils.IntegerMassTable

  private def calcPeptidePrefixMasses(peptide: String): Vector[Int] =
    peptide.scanLeft(0){ case (mass, aminoAcid) => mass + IntegerMassTable(aminoAcid) }.toVector

  def calcTheoreticalSpectrum(peptide: String): List[Int] = {
    val prefixMasses: Vector[Int] = calcPeptidePrefixMasses(peptide)
    val n: Int = peptide.length
    (0 until n).foldLeft(List(0)){ case (spectrum, startIx) =>
      (startIx + 1 to n).foldLeft(spectrum){ case (acc, endIx) =>
        val subpeptideMass: Int = prefixMasses(endIx) - prefixMasses(startIx)
        subpeptideMass :: acc
      }
    }.sorted
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val peptide: String = reader.next()
    val result: List[Int] = calcTheoreticalSpectrum(peptide)
    println(result.mkString(" "))
  }
}
