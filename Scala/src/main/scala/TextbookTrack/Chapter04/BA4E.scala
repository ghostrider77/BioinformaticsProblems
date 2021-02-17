package TextbookTrack.Chapter04

object BA4E {
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

  private def isPeptideConsistentWithExperimentalSpectrum(peptide: Peptide,
                                                          experimentalSpectrum: Map[Int, Int]): Boolean = {
    val linearSpectrum: Map[Int, Int] = calcTheoreticalLinearSpectrum(peptide)
    linearSpectrum.forall{ case (mass, count) => experimentalSpectrum.getOrElse(mass, 0) >= count }
  }

  private def expand(peptides: List[Peptide], aminoAcidMasses: Set[Int]): Iterator[Peptide] =
    peptides.iterator.flatMap{ peptide => aminoAcidMasses.map(mass => peptide.copy(mass :: peptide.masses)) }

  def cyclopeptideSequencing(experimentalSpectrum: Map[Int, Int]): List[Peptide] = {
    val aminoAcidMasses: Set[Int] = IntegerMassTable.values.toSet
    val parentMass: Int = experimentalSpectrum.keysIterator.max

    @tailrec
    def loop(acceptedPeptides: List[Peptide], currentPeptides: List[Peptide]): List[Peptide] = {
      if (currentPeptides.isEmpty) acceptedPeptides
      else {
        val candidatePeptides: Iterator[Peptide] = expand(currentPeptides, aminoAcidMasses)
        val (updatedAcceptedCandidates, candidates): (List[Peptide], List[Peptide]) =
          candidatePeptides.foldLeft((acceptedPeptides, List.empty[Peptide])){
            case ((accepted, candidates), peptide) =>
              if (peptide.totalMass == parentMass) {
                val cycloSpectrum: Map[Int, Int] = calcTheoreticalCyclicSpectrum(peptide)
                if (cycloSpectrum == experimentalSpectrum) (peptide :: accepted, candidates)
                else (accepted, candidates)
              }
              else if (isPeptideConsistentWithExperimentalSpectrum(peptide, experimentalSpectrum))
                (accepted, peptide :: candidates)
              else (accepted, candidates)
          }
        loop(updatedAcceptedCandidates, candidates)
      }
    }

    loop(Nil, List(Peptide(Nil)))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val experimentalSpectrum: Map[Int, Int] = convertToIntList(reader.next())
    val result: List[Peptide] = cyclopeptideSequencing(experimentalSpectrum)
    println(result.mkString(" "))
  }
}
