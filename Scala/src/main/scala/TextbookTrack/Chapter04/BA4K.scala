package TextbookTrack.Chapter04

object BA4K {
  import scala.language.implicitConversions
  import TextbookTrack.Utils.IntegerMassTable

  final case class Peptide(masses: List[Int]) {
    override def toString: String = masses.mkString("-")

    val length: Int = masses.length
    val totalMass: Int = masses.sum
  }

  object Peptide {
    def apply(peptide: String): Peptide = {
      val masses: List[Int] = peptide.map(IntegerMassTable(_)).toList
      Peptide(masses)
    }
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

  def calcConsistencyScore(peptide: Peptide, experimentalSpectrum: Map[Int, Int]): Int = {
    val linearSpectrum: Map[Int, Int] = calcTheoreticalLinearSpectrum(peptide)
    linearSpectrum.foldLeft(0){
      case (score, (mass, count)) => score + math.min(experimentalSpectrum.getOrElse(mass, 0), count)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val peptide: Peptide = Peptide(reader.next())
    val experimentalSpectrum: Map[Int, Int] = convertToIntList(reader.next())
    val result: Int = calcConsistencyScore(peptide, experimentalSpectrum)
    println(result)
  }
}
