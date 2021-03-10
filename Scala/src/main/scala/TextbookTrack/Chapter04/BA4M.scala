package TextbookTrack.Chapter04

object BA4M {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def calcOccurrences[T](xs: List[T]): Map[T, Int] =
    xs.groupMapReduce(identity)(_ => 1)(_ + _)

  private def readPositiveDifferences(line: String): Map[Int, Int] = {
    val differences: List[Int] = convertToIntList(line)
    calcOccurrences(differences.filter(_ > 0))
  }

  private def calcPositiveDifferences(xs: List[Int], elem: Int): Map[Int, Int] =
    calcOccurrences(xs.map(x => math.abs(elem - x)))

  private def isSubset(delta: Map[Int, Int], diffs: Map[Int, Int]): Boolean =
    delta.forall{ case (item, count) => diffs.getOrElse(item, 0) >= count }

  private def remove(differences: Map[Int, Int], items: Map[Int, Int]): Map[Int, Int] =
    items.foldLeft(differences){
      case (acc, (item, count)) =>
        val currentCount: Int = acc.getOrElse(item, 0)
        if (currentCount == count) acc.removed(item)
        else acc.updated(item, currentCount - count)
    }

  private def insertLargestValue(xs: List[Int], differences: Map[Int, Int], y: Int, m: Int): Option[List[Int]] = {
    val delta: Map[Int, Int] = calcPositiveDifferences(xs, y)
    if (isSubset(delta, differences)) {
      val updatedDifferences: Map[Int, Int] = remove(differences, delta)
      solve(y :: xs, updatedDifferences, m)
    } else None
  }

  private def solve(xs: List[Int], differences: Map[Int, Int], m: Int): Option[List[Int]] = {
    if (differences.isEmpty) Some(xs)
    else {
      val y: Int = differences.keysIterator.max
      lazy val result1: Option[List[Int]] = insertLargestValue(xs, differences, y, m)
      lazy val result2: Option[List[Int]] = insertLargestValue(xs, differences, m - y, m)
      if (result1.nonEmpty) result1
      else if (result2.nonEmpty) result2
      else None
    }
  }

  def solveTurnpikeProblem(differences: Map[Int, Int]): List[Int] = {
    val m: Int = differences.keysIterator.max
    val updatedDifferences: Map[Int, Int] = remove(differences, Map(m -> 1))
    val distances: List[Int] = List(m, 0)
    solve(distances, updatedDifferences, m) match {
      case Some(result) => result.sorted
      case None => throw new Exception("No solution has been found.")
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val differences: Map[Int, Int] = readPositiveDifferences(reader.next())
    val result: List[Int] = solveTurnpikeProblem(differences)
    println(result.mkString(" "))
  }
}
