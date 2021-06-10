package TextbookTrack.Chapter08

object BA8B {
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

  def calcDistortion(points: List[Point], centers: List[Point]): Double =
    points.foldLeft(0.0)((d, point) => d + math.pow(distanceFromCenters(point, centers), 2)) / points.length

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (k, _): (Int, Int) = readParameters(reader.next())
    val centers: List[Point] = reader.take(k).map(convertToNumber(_, _.toDouble)).toList
    val _: String = reader.next()
    val points: List[Point] = reader.map(convertToNumber(_, _.toDouble)).toList
    val result: Double = calcDistortion(points, centers)
    println(result)
  }
}
