package TextbookTrack.Chapter04

object BA4G {
  import scala.language.implicitConversions
  import scala.annotation.tailrec
  import TextbookTrack.Utils.IntegerMassTable

  final case class Peptide(masses: List[Int]) {
    override def toString: String = masses.mkString("-")

    val length: Int = masses.length
    val totalMass: Int = masses.sum
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

  private def calcTheoreticalCyclicSpectrum(peptide: Peptide): List[Int] = {
    val prefixMasses: Vector[Int] = calcPeptidePrefixMasses(peptide)
    val peptideMass: Int = prefixMasses.last
    val n: Int = peptide.length
    (0 until n).foldLeft(List(0)){ case (cycloSpectrum, startIx) =>
      (startIx + 1 to n).foldLeft(cycloSpectrum){ case (acc, endIx) =>
        val subpeptideMass: Int = prefixMasses(endIx) - prefixMasses(startIx)
        if (startIx > 0 && endIx < n) (peptideMass - subpeptideMass) :: subpeptideMass :: acc
        else subpeptideMass :: acc
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

  private def expand(peptides: List[Peptide], aminoAcidMasses: Set[Int]): Iterator[Peptide] =
    peptides.iterator.flatMap{ peptide => aminoAcidMasses.map(mass => peptide.copy(mass :: peptide.masses)) }

  private def trimLeaderboard(leaderboard: List[Peptide],
                              experimentalSpectrum: Map[Int, Int],
                              limit: Int): List[Peptide] = {
    if (leaderboard.isEmpty) leaderboard
    else {
      val scoredPeptides: List[(Peptide, Int)] = leaderboard.map{
        peptide => (peptide, calcConsistencyScore(peptide, experimentalSpectrum, calcTheoreticalLinearSpectrum))
      }
      val tieScore: Int = scoredPeptides.sortBy{ case (_, score) => score }(Ordering[Int].reverse).take(limit).last._2
      scoredPeptides.withFilter{ case (_, score) => score >= tieScore }.map{ case (peptide, _) => peptide }
    }
  }

  def leaderboardCyclopeptideSequencing(experimentalSpectrum: Map[Int, Int], limit: Int): Peptide = {
    val aminoAcidMasses: Set[Int] = IntegerMassTable.values.toSet
    val parentMass: Int = experimentalSpectrum.keysIterator.max

    @tailrec
    def loop(leader: Peptide, leaderScore: Int, leaderboard: List[Peptide]): Peptide = {
      if (leaderboard.isEmpty) leader
      else {
        val candidatePeptides: Iterator[Peptide] = expand(leaderboard, aminoAcidMasses)
        val (updatedLeader, updatedHighScore, updatedLeaderboard): (Peptide, Int, List[Peptide]) =
          candidatePeptides.foldLeft((leader, leaderScore, List.empty[Peptide])){
            case (acc @ (currentLeader, currentHighScore , peptides), peptide) =>
              if (peptide.totalMass == parentMass) {
                val score: Int = calcConsistencyScore(peptide, experimentalSpectrum, calcTheoreticalCyclicSpectrum)
                if (score > currentHighScore) (peptide, score, peptides)
                else acc
              }
              else if (peptide.totalMass < parentMass) (currentLeader, currentHighScore, peptide :: peptides)
              else acc
          }
        val trimmedBoard: List[Peptide] = trimLeaderboard(updatedLeaderboard, experimentalSpectrum, limit)
        loop(updatedLeader, updatedHighScore, trimmedBoard)
      }
    }

    val emptyPeptide = Peptide(Nil)
    loop(emptyPeptide, 0, List(emptyPeptide))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val limit: Int = reader.next().toInt
    val experimentalSpectrum: Map[Int, Int] = convertToIntList(reader.next())
    val result: Peptide = leaderboardCyclopeptideSequencing(experimentalSpectrum, limit)
    println(result)
  }
}
