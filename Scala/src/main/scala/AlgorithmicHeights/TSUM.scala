package AlgorithmicHeights

object TSUM {
  import scala.annotation.tailrec

  sealed trait Result
  case class IndexPair(i: Int, j: Int) extends Result {
    override def toString: String = s"$i $j"
  }
  case object NotFound extends Result {
    override def toString: String = "-1"
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(k, n) => (k, n)
    case _ => throw new Exception("Malformed input.")
  }

  def findZeroSumIndexPairs(arrays: List[List[Int]]): List[Result] = {
    def solve2Sum(array: List[Int]): Result = {
      @tailrec
      def loop(negativeTargetIndices: Map[Int, Int], xs: List[(Int, Int)]): Result = xs match {
        case Nil => NotFound
        case (item, ix) :: xss =>
          negativeTargetIndices.get(item) match {
            case None => loop(negativeTargetIndices.updated(-item, ix), xss)
            case Some(jy) => IndexPair(jy + 1, ix + 1)
          }
      }
      loop(Map(), array.zipWithIndex)
    }

    arrays.map(solve2Sum)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (k, _): (Int, Int) = readParameters(reader.next())
    val arrays: List[List[Int]] = reader.take(k).map(convertToIntList).toList
    val result: List[Result] = findZeroSumIndexPairs(arrays)
    result.foreach(println)
  }
}
