package AlgorithmicHeights

object Bins {
  import scala.annotation.tailrec

  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  def findElemsInSortedArray(array: Vector[Int], queries: Vector[Int], n: Int): Vector[Int] = {
    @tailrec
    def binarySearch(item: Int, left: Int, right: Int): Int = {
      if (left > right) -1
      else {
        val middleIx: Int = (left + right) / 2
        val middleElem: Int = array(middleIx)
        if (middleElem == item) middleIx + 1
        else if (middleElem < item) binarySearch(item, middleIx + 1, right)
        else binarySearch(item, left, middleIx - 1)
      }
    }

    queries.map(binarySearch(_, 0, n - 1))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val _: Int = reader.next().toInt
    val array: Vector[Int] = convertToIntVector(reader.next())
    val queries: Vector[Int] = convertToIntVector(reader.next())
    val result: Vector[Int] = findElemsInSortedArray(array, queries, n)
    println(result.mkString(" "))
  }
}
