package TextbookTrack.Chapter11

object BA11G {
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

  private def findHighestScoringPeptideInProteome(spectrum: Vector[Int],
                                                  proteome: String,
                                                  massTable: Map[Char, Int]): (String, Int) = {
    val massValues: Set[Int] = massTable.valuesIterator.toSet
    val totalMass: Int = spectrum.length
    val minPeptideLength: Int = math.ceil(totalMass / massValues.max).toInt
    val maxPeptideLength: Int = totalMass / massValues.min
    (minPeptideLength to maxPeptideLength).foldLeft(("", Int.MinValue)){
      case (acc @ (_, bestScore), k) =>
        val scores: Iterator[(String, Int)] =
          proteome
            .sliding(k)
            .map(peptide => (peptide, calcPeptideScore(peptide, spectrum, totalMass, massTable)))
        val (peptide, score): (String, Int) = scores.maxBy{ case (_, score) => score }
        if (score > bestScore) (peptide, score) else acc
    }
  }

  def findPeptideSpectrumMatchPairs(spectra: Vector[Vector[Int]],
                                    proteome: String,
                                    threshold: Int,
                                    massTable: Map[Char, Int]): Set[String] = {
    spectra.foldLeft(Set.empty[String]){
      case (peptides, spectrum) =>
        val (peptide, score): (String, Int) = findHighestScoringPeptideInProteome(spectrum, proteome, massTable)
        if (score >= threshold) peptides + peptide else peptides
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val lines: Vector[String] = reader.toVector
    val nrLines: Int = lines.length
    val spectra: Vector[Vector[Int]] = lines.dropRight(2).map(convertToIntVector)
    val proteome: String = lines(nrLines - 2)
    val threshold: Int = lines.last.toInt
    val result: Set[String] = findPeptideSpectrumMatchPairs(spectra, proteome, threshold, IntegerMassTable)
    result.foreach(println)
  }
}
