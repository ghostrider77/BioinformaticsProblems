package AlgorithmicHeights

object SUM3 {
  import scala.annotation.tailrec

  sealed trait Result
  case class IndexTriples(i: Int, j: Int, k: Int) extends Result {
    override def toString: String = s"$i $j $k"
  }
  case object NotFound extends Result {
    override def toString: String = "-1"
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(k, n) => (k, n)
    case _ => throw new Exception("Malformed input.")
  }

  private def solve2Sum(array: List[Int], target: Int, targetIx: Int): Option[IndexTriples] = {
    @tailrec
    def loop(negativeTargetIndices: Map[Int, Int], xs: List[(Int, Int)]): Option[IndexTriples] = xs match {
      case Nil => None
      case (item, ix) :: xss =>
        negativeTargetIndices.get(item) match {
          case None => loop(negativeTargetIndices.updated(target - item, ix), xss)
          case Some(jy) => Some(IndexTriples(targetIx + 1, jy + 1, ix + 1))
        }
    }
    loop(Map(), array.zipWithIndex.drop(targetIx + 1))
  }

  def findZeroSumIndexPairs(arrays: List[List[Int]]): List[Result] = {
    def solve3Sum(array: List[Int]): Result = {
      array
        .iterator
        .zipWithIndex
        .flatMap{ case (item, ix) => solve2Sum(array, -item, ix) }
        .nextOption() match {
        case None => NotFound
        case Some(result) => result
      }
    }

    arrays.map(solve3Sum)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (k, _): (Int, Int) = readParameters(reader.next())
    val arrays: List[List[Int]] = reader.take(k).map(convertToIntList).toList
    val result: List[Result] = findZeroSumIndexPairs(arrays)
    result.foreach(println)
  }
}
