package TextbookTrack.Chapter08

object BA8A {
  import scala.reflect.ClassTag

  type Point = List[Double]

  private def convertToNumber[T: ClassTag](line: String, converter: String => T): List[T] =
    line.split(" ").map(converter).toList

  private def readParameters(line: String): (Int, Int) = convertToNumber(line, _.toInt) match {
    case List(k, m) => (k, m)
    case _ => throw new Exception("Unexpected number of parameters.")
  }

  private def euclideanDistance(p: Point, q: Point): Double =
    math.sqrt(p.lazyZip(q).foldLeft(0.0){ case (d, (pi, qi)) => d + math.pow(pi - qi, 2) })

  private def distanceFromCenters(point: Point, centers: List[Point]): Double =
    centers.map(euclideanDistance(_, point)).min

  private def findIndexThatMaximizesDistance(points: List[Point], centerIndices: List[Int]): Int = {
    val centers: List[Point] = centerIndices.map(points)
    val (maxIx, _): (Int, Double) = points.zipWithIndex.foldLeft((0, 0.0)){
      case (acc @ (_, maxDistance), (point, ix)) =>
        if (centerIndices.contains(ix)) acc
        else {
          val d: Double = distanceFromCenters(point, centers)
          if (d >= maxDistance) (ix, d) else acc
        }
    }
    maxIx
  }

  def farthestFirstTraversal(points: List[Point], k: Int): List[Point] = {
    val centerIndices: List[Int] = (0 until k - 1).foldLeft(List(0)){
      case (indices, _) => findIndexThatMaximizesDistance(points, indices) :: indices
    }
    centerIndices.map(points).reverse
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (k, _): (Int, Int) = readParameters(reader.next())
    val points: List[Point] = reader.map(convertToNumber(_, _.toDouble)).toList
    val result: List[Point] = farthestFirstTraversal(points, k)
    result.foreach(point => println(point.mkString(" ")))
  }
}
