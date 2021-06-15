package TextbookTrack.Chapter08

object BA8D {
  import scala.reflect.ClassTag
  import scala.annotation.tailrec

  type Point = List[Double]

  private val NumberOfIterations: Int = 100

  private def convertToNumber[T: ClassTag](line: String, converter: String => T): List[T] =
    line.split(" ").map(converter).toList

  private def readParameters(line: String): (Int, Int) = convertToNumber(line, _.toInt) match {
    case List(k, m) => (k, m)
    case _ => throw new Exception("Unexpected number of parameters.")
  }

  private def euclideanDistance(p: Point, q: Point): Double =
    math.sqrt(p.lazyZip(q).foldLeft(0.0){ case (d, (pi, qi)) => d + math.pow(pi - qi, 2) })

  private def assignPointsToCenters(points: List[Point], centers: List[Point], beta: Double): List[List[Double]] = {
    val responsibilityMatrix: List[List[Double]] =
      centers.map{ center => points.map(point => math.exp(-beta * euclideanDistance(center, point))) }
    val columnSums: List[Double] = responsibilityMatrix.transpose.map(_.sum)
    responsibilityMatrix.map(row => row.zip(columnSums).map{ case (elem, sum) => elem / sum })
  }

  private def dotProduct(v: List[Double], w: List[Double]): Double =
    v.lazyZip(w).foldLeft(0.0){ case (s, (a, b)) => s + a * b }

  private def recalculateCenters(points: List[Point], responsibilityMatrix: List[List[Double]]): List[Point] = {
    val dataColumns: List[List[Double]] = points.transpose
    val rowSums: List[Double] = responsibilityMatrix.map(_.sum)
    responsibilityMatrix
      .lazyZip(rowSums)
      .map{ case (responsibilityRow, sum) => dataColumns.map(dotProduct(responsibilityRow, _) / sum) }
  }

  def runSoftKMeansClustering(points: List[Point], k: Int, beta: Double): List[Point] = {
    @tailrec
    def loop(centers: List[Point], nrIterations: Int): List[Point] = {
      if (nrIterations == NumberOfIterations) centers
      else {
        val responsibilityMatrix: List[List[Double]] = assignPointsToCenters(points, centers, beta)
        val updatedCenters: List[Point] = recalculateCenters(points, responsibilityMatrix)
        loop(updatedCenters, nrIterations + 1)
      }
    }

    loop(points.take(k), 0)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (k, _): (Int, Int) = readParameters(reader.next())
    val beta: Double = reader.next().toDouble
    val points: List[Point] = reader.map(convertToNumber(_, _.toDouble)).toList
    val result: List[Point] = runSoftKMeansClustering(points, k, beta)
    result.foreach(point => println(point.mkString(" ")))
  }
}
