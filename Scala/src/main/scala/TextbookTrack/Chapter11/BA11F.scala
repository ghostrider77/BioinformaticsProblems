package TextbookTrack.Chapter11

object BA11F {
  import TextbookTrack.Utils.IntegerMassTable

  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  private def calcPeptideScore(peptide: String,
                               spectrum: Vector[Int],
                               totalMass: Int,
                               massTable: Map[Char, Int]): Int = {
    val prefixMasses: Vector[Int] = peptide.scanLeft(0)((acc, aminoAcid) => acc + massTable(aminoAcid)).toVector
    if (prefixMasses.last != totalMass) Int.MinValue
    else prefixMasses.drop(1).foldLeft(0)((acc, ix) => acc + spectrum(ix - 1))
  }

  def findHighestScoringPeptideInProteome(spectrum: Vector[Int],
                                          proteome: String,
                                          massTable: Map[Char, Int]): String = {
    val massValues: Set[Int] = massTable.valuesIterator.toSet
    val totalMass: Int = spectrum.length
    val minPeptideLength: Int = math.ceil(totalMass / massValues.max).toInt
    val maxPeptideLength: Int = totalMass / massValues.min
    val (bestPeptide, _): (String, Int) = (minPeptideLength to maxPeptideLength).foldLeft(("", Int.MinValue)){
      case (acc @ (_, bestScore), k) =>
        val scores: Iterator[(String, Int)] =
          proteome
            .sliding(k)
            .map(peptide => (peptide, calcPeptideScore(peptide, spectrum, totalMass, massTable)))
        val (peptide, score): (String, Int) = scores.maxBy{ case (_, score) => score }
        if (score > bestScore) (peptide, score) else acc
    }
    bestPeptide
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val spectrum: Vector[Int] = convertToIntVector(reader.next())
    val proteome: String = reader.next()
    val result: String = findHighestScoringPeptideInProteome(spectrum, proteome, IntegerMassTable)
    println(result)
  }
}
