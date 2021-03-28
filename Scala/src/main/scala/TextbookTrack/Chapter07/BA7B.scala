package TextbookTrack.Chapter07

object BA7B {
  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  private def readDistanceMatrix(reader: Iterator[String], nrLeaves: Int): Vector[Vector[Int]] =
    reader.take(nrLeaves).map(convertToIntVector).toVector

  def calcLimbLength(distances: Vector[Vector[Int]], leafJ: Int, nrLeaves: Int): Int = {
    val otherLeaves: List[Int] = (0 until nrLeaves).filterNot(_ == leafJ).toList
    otherLeaves
      .iterator
      .flatMap(leafI => otherLeaves.map(leafK => (leafI, leafK)))
      .foldLeft(Int.MaxValue) {
        case (limbLength, (leafI, leafK)) =>
          val candidate: Int = (distances(leafI)(leafJ) + distances(leafJ)(leafK) - distances(leafI)(leafK)) / 2
          math.min(candidate, limbLength)
      }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrLeaves: Int = reader.next().toInt
    val leaf: Int = reader.next().toInt
    val distances: Vector[Vector[Int]] = readDistanceMatrix(reader, nrLeaves)
    val result: Int = calcLimbLength(distances, leaf, nrLeaves)
    println(result)
  }
}
