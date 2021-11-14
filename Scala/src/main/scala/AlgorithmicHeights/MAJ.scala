package AlgorithmicHeights

object MAJ {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(k, n) => (k, n)
    case _ => throw new Exception("Malformed input.")
  }

  def calcMajorityElements(arrays: List[List[Int]], n: Int): List[Int] = {
    def majorityElement(array: List[Int]): Int = {
      if (array.isEmpty) -1
      else {
        val counter: Map[Int, Int] = array.groupMapReduce(identity)(_ => 1)(_ + _)
        val (mostCommon, count): (Int, Int) = counter.maxBy{ case (_, count) => count }
        if (count > n / 2) mostCommon else -1
      }
    }

    arrays.map(majorityElement)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (k, n): (Int, Int) = readParameters(reader.next())
    val arrays: List[List[Int]] = reader.take(k).map(convertToIntList).toList
    val result: List[Int] = calcMajorityElements(arrays, n)
    println(result.mkString(" "))
  }
}
