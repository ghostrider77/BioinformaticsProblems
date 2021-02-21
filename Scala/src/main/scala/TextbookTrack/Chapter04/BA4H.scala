package TextbookTrack.Chapter04

object BA4H {
  import scala.language.implicitConversions
  import scala.collection.mutable.{Map => MutableMap}

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private implicit def toMassCounts(masses: List[Int]): Map[Int, Int] =
    masses.groupBy(identity).view.mapValues(_.length).toMap

  def calcConvolutionOfSpectrum(spectrum: Map[Int, Int]): List[Int] = {
    val convolution: MutableMap[Int, Int] = MutableMap()
    for {
      (mass1, count1) <- spectrum
      (mass2, count2) <- spectrum
      difference: Int = mass1 - mass2
      if difference > 0
    } convolution(difference) = convolution.getOrElse(difference, 0) + count1 * count2

    convolution
      .toList
      .sortBy{ case (_, count) => count }(Ordering[Int].reverse)
      .flatMap{ case (massDifference, count) => List.fill(count)(massDifference) }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val spectrum: Map[Int, Int] = convertToIntList(reader.next())
    val result: List[Int] = calcConvolutionOfSpectrum(spectrum)
    println(result.mkString(" "))
  }
}
