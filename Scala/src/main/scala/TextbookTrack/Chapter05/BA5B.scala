package TextbookTrack.Chapter05

object BA5B {
  type WeightMatrix = Vector[Vector[Int]]

  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  private def readParameters(line: String): (Int, Int) = convertToIntVector(line) match {
    case Vector(n, m) => (n, m)
    case _ => throw new Exception("Unexpected number of parameters.")
  }

  private def readWeightMatrix(lines: Iterator[String]): WeightMatrix = lines.map(convertToIntVector).toVector

  def calcLongestPath(n: Int, m: Int, downWeights: WeightMatrix, rightWeights: WeightMatrix): Int = {
    val longestPath: Array[Array[Int]] = Array.fill(n + 1, m + 1)(0)
    (1 to n).foreach(ix => longestPath(ix)(0) = longestPath(ix - 1)(0) + downWeights(ix - 1)(0))
    (1 to m).foreach(jy => longestPath(0)(jy) = longestPath(0)(jy - 1) + rightWeights(0)(jy - 1))

    for {
      ix <- 1 to n
      jy <- 1 to m
    } {
      val pathDown: Int = longestPath(ix - 1)(jy) + downWeights(ix - 1)(jy)
      val pathRight: Int = longestPath(ix)(jy - 1) + rightWeights(ix)(jy - 1)
      longestPath(ix)(jy) = math.max(pathDown, pathRight)
    }
    longestPath(n)(m)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (n, m): (Int, Int) = readParameters(reader.next())
    val down: WeightMatrix = readWeightMatrix(reader.take(n))
    val _: String = reader.next()
    val right: WeightMatrix = readWeightMatrix(reader.take(n + 1))
    val result: Int = calcLongestPath(n, m, down, right)
    println(result)
  }
}
