package TextbookTrack.Chapter04

object BA4L {
  import scala.language.implicitConversions
  import TextbookTrack.Utils.IntegerMassTable

  final case class Peptide(peptide: String) {
    val masses: List[Int] = peptide.map(IntegerMassTable(_)).toList

    override def toString: String = peptide

    val length: Int = masses.length
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private implicit def toMassCounts(masses: List[Int]): Map[Int, Int] =
    masses.groupBy(identity).view.mapValues(_.length).toMap

  private def calcPeptidePrefixMasses(peptide: Peptide): Vector[Int] =
    peptide.masses.reverseIterator.scanLeft(0){ _ + _ }.toVector

  private def calcTheoreticalLinearSpectrum(peptide: Peptide): List[Int] = {
    val prefixMasses: Vector[Int] = calcPeptidePrefixMasses(peptide)
    val n: Int = peptide.length
    (0 until n).foldLeft(List(0)){ case (spectrum, startIx) =>
      (startIx + 1 to n).foldLeft(spectrum){ case (acc, endIx) =>
        val subpeptideMass: Int = prefixMasses(endIx) - prefixMasses(startIx)
        subpeptideMass :: acc
      }
    }.sorted
  }

  private def calcConsistencyScore(peptide: Peptide,
                                   experimentalSpectrum: Map[Int, Int],
                                   spectrumCalculator: Peptide => Map[Int, Int]): Int = {
    val spectrum: Map[Int, Int] = spectrumCalculator(peptide)
    spectrum.foldLeft(0){
      case (score, (mass, count)) => score + math.min(experimentalSpectrum.getOrElse(mass, 0), count)
    }
  }

  def trimLeaderboard(leaderboard: List[Peptide], experimentalSpectrum: Map[Int, Int], limit: Int): List[Peptide] = {
    if (leaderboard.isEmpty) leaderboard
    else {
      val scoredPeptides: List[(Peptide, Int)] = leaderboard.map{
        peptide => (peptide, calcConsistencyScore(peptide, experimentalSpectrum, calcTheoreticalLinearSpectrum))
      }.sortBy{ case (_, score) => score }(Ordering[Int].reverse)
      val tieScore: Int = scoredPeptides.take(limit).last._2
      scoredPeptides.withFilter{ case (_, score) => score >= tieScore }.map{ case (peptide, _) => peptide }
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val leaderboard: List[Peptide] = reader.next().split(" ").map(Peptide).toList
    val experimentalSpectrum: Map[Int, Int] = convertToIntList(reader.next())
    val limit: Int = reader.next().toInt
    val result: List[Peptide] = trimLeaderboard(leaderboard, experimentalSpectrum, limit)
    println(result.mkString(" "))
  }
}
