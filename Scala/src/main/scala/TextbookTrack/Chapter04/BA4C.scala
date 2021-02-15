package TextbookTrack.Chapter04

object BA4C {
  import TextbookTrack.Utils.IntegerMassTable

  private def calcPeptidePrefixMasses(peptide: String): Vector[Int] =
    peptide.scanLeft(0){ case (mass, aminoAcid) => mass + IntegerMassTable(aminoAcid) }.toVector

  def calcTheoreticalSpectrum(cyclicPeptide: String): List[Int] = {
    val prefixMasses: Vector[Int] = calcPeptidePrefixMasses(cyclicPeptide)
    val peptideMass: Int = prefixMasses.last
    val n: Int = cyclicPeptide.length
    (0 until n).foldLeft(List(0)){ case (cycloSpectrum, startIx) =>
      (startIx + 1 to n).foldLeft(cycloSpectrum){ case (acc, endIx) =>
        val subpeptideMass: Int = prefixMasses(endIx) - prefixMasses(startIx)
        if (startIx > 0 && endIx < n) (peptideMass - subpeptideMass) :: subpeptideMass :: acc
        else subpeptideMass :: acc
      }
    }.sorted
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val cyclicPeptide: String = reader.next()
    val result: List[Int] = calcTheoreticalSpectrum(cyclicPeptide)
    println(result.mkString(" "))
  }
}
