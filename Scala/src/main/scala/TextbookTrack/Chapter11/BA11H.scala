package TextbookTrack.Chapter11

object BA11H {
  import TextbookTrack.Utils.IntegerMassTable
  import scala.collection.mutable.{Map => MutableMap}

  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  def calcSpectralDictionarySize(spectrum: Vector[Int],
                                 massTable: Map[Char, Int],
                                 threshold: Int,
                                 maxScore: Int): Int = {
    val aminoAcidMasses: List[Int] = massTable.valuesIterator.toList
    val m: Int = spectrum.length
    val cache: MutableMap[(Int, Int), Int] = MutableMap()

    def solve(ix: Int, jy: Int): Int = {
      if (ix == 0 && jy == 0) 1
      else if (ix <= 0) 0
      else {
        def score: Int = aminoAcidMasses.foldLeft(0)((acc, mass) => acc + solve(ix - mass, jy - spectrum(ix - 1)))
        cache.getOrElseUpdate((ix, jy), score)
      }
    }

    (threshold to maxScore).foldLeft(0)((acc, t) => acc + solve(m, t))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val spectrum: Vector[Int] = convertToIntVector(reader.next())
    val threshold: Int = reader.next().toInt
    val maxScore: Int = reader.next().toInt
    val result: Int = calcSpectralDictionarySize(spectrum, IntegerMassTable, threshold, maxScore)
    println(result)
  }
}
