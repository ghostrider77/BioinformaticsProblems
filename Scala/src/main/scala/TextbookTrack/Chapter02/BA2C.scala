package TextbookTrack.Chapter02

object BA2C {
  final case class ProfileColumn(A: Double, C: Double, G: Double, T: Double) {
    def apply(c: Char): Double = c match {
      case 'A' => A
      case 'C' => C
      case 'G' => G
      case 'T' => T
      case _ => throw new Exception("Unknown nucleotide.")
    }
  }

  private def convertToFloatList(line: String): List[Double] = line.split(" ").map(_.toDouble).toList

  def readProfileMatrix(lines: Iterator[String]): List[ProfileColumn] = {
    val rows: List[List[Double]] = lines.map(convertToFloatList).toList
    rows.transpose.map{
      case List(a, c, g, t) => ProfileColumn(a, c, g, t)
      case _ => throw new Exception("4 values are expected in each column.")
    }
  }

  private def calcKMerProbabilty(kMer: String, profileMatrix: List[ProfileColumn]): Double =
    kMer.lazyZip(profileMatrix).foldLeft(1.0){ case (acc, (nucleotide, column)) => acc * column(nucleotide) }

  def profileMostProbableKMer(text: String, profileMatrix: List[ProfileColumn], k: Int): String = {
    val (_, mostProbableKMer): (Double, String) = text.sliding(k).foldLeft((0.0, text.take(k))){
      case (acc @ (maxProbability, _), kMer) =>
        val p: Double = calcKMerProbabilty(kMer, profileMatrix)
        if (p > maxProbability) (p, kMer) else acc
    }
    mostProbableKMer
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next()
    val k: Int = reader.next().toInt
    val matrixColumns: List[ProfileColumn] = readProfileMatrix(reader)
    val result: String = profileMostProbableKMer(text, matrixColumns, k)
    println(result)
  }
}
