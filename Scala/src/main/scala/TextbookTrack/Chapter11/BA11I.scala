package TextbookTrack.Chapter11

object BA11I {
  import TextbookTrack.Utils.IntegerMassTable
  import scala.collection.mutable.{Map => MutableMap}

  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  def calcProbabilityOfSpectralDictionary(spectrum: Vector[Int],
                                          massTable: Map[Char, Int],
                                          threshold: Int,
                                          maxScore: Int): Double = {
    val aminoAcidMasses: List[Int] = massTable.valuesIterator.toList
    val nrAminoAcids: Int = massTable.size
    val m: Int = spectrum.length
    val cache: MutableMap[(Int, Int), Double] = MutableMap()

    def solve(ix: Int, jy: Int): Double = {
      if (ix == 0 && jy == 0) 1.0
      else if (ix <= 0 || jy < 0) 0.0
      else {
        def score: Double =
          aminoAcidMasses.foldLeft(0.0)((acc, mass) => acc + solve(ix - mass, jy - spectrum(ix - 1))) / nrAminoAcids
        cache.getOrElseUpdate((ix, jy), score)
      }
    }

    (threshold to maxScore).foldLeft(0.0)((acc, t) => acc + solve(m, t))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val spectrum: Vector[Int] = convertToIntVector(reader.next())
    val threshold: Int = reader.next().toInt
    val maxScore: Int = reader.next().toInt
    val result: Double = calcProbabilityOfSpectralDictionary(spectrum, IntegerMassTable, threshold, maxScore)
    println(result)
  }
}
