package TextbookTrack.Chapter08

object BA8C {
  import scala.reflect.ClassTag
  import scala.annotation.tailrec

  type Point = Vector[Double]

  private def convertToNumber[T: ClassTag](line: String, converter: String => T): Vector[T] =
    line.split(" ").map(converter).toVector

  private def readParameters(line: String): (Int, Int) = convertToNumber(line, _.toInt) match {
    case Vector(k, m) => (k, m)
    case _ => throw new Exception("Unexpected number of parameters.")
  }

  private def euclideanDistance(p: Point, q: Point): Double =
    math.sqrt(p.lazyZip(q).foldLeft(0.0){ case (d, (pi, qi)) => d + math.pow(pi - qi, 2) })

  private def selectClosestCenter(point: Point, centers: List[Point]): Int =
    centers
      .iterator
      .map(euclideanDistance(point, _))
      .zipWithIndex
      .minBy{ case (distance, _) => distance }
      ._2

  private def assignPointsToCenters(points: List[Point], centers: List[Point]): List[Int] =
    points.map(selectClosestCenter(_, centers))

  private def recalculateCenters(points: List[Point], centerAssignment: List[Int], k: Int): List[Point] =
    (0 until k).map{
      clusterId =>
        val clusterPoints: List[Point] =
          points
            .lazyZip(centerAssignment)
            .withFilter{ case (_, ix) => ix == clusterId }
            .map{ case (point, _) => point }
            .toList
        val clusterSize: Int = clusterPoints.length
        clusterPoints.transpose.map(_.sum / clusterSize).toVector
    }.toList

  private def areSameCenters(centers: List[Point], updatedCenters: List[Point]): Boolean =
    centers.lazyZip(updatedCenters).forall{euclideanDistance(_, _) <= 1e-12 }

  def runKMeansClustering(points: List[Point], k: Int): List[Point] = {
    @tailrec
    def loop(centers: List[Point]): List[Point] = {
      val centerAssignment: List[Int] = assignPointsToCenters(points, centers)
      val updatedCenters: List[Point] = recalculateCenters(points, centerAssignment, k)
      if (areSameCenters(centers, updatedCenters)) updatedCenters
      else loop(updatedCenters)
    }

    loop(points.take(k))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (k, _): (Int, Int) = readParameters(reader.next())
    val points: List[Point] = reader.map(convertToNumber(_, _.toDouble)).toList
    val result: List[Point] = runKMeansClustering(points, k)
    result.foreach(point => println(point.mkString(" ")))
  }
}
