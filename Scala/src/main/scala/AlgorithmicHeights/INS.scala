package AlgorithmicHeights

object INS {
  import scala.annotation.tailrec

  private def convertToIntArray(line: String): Array[Int] = line.split(" ").map(_.toInt)

  def calcNrSwapsInInsertionSort(array: Array[Int], n: Int): Int = {
    @tailrec
    def moveElemToItsPlace(nrSwaps: Int, k: Int): Int = {
      if (k == 0 || array(k) >= array(k - 1)) nrSwaps
      else {
        swapElems(k, k - 1)
        moveElemToItsPlace(nrSwaps + 1, k - 1)
      }
    }

    def swapElems(i: Int, j: Int): Unit = {
      val elem: Int = array(i)
      array(i) = array(j)
      array(j) = elem
    }

    (1 until n).foldLeft(0)((acc, ix) => moveElemToItsPlace(acc, ix))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val array: Array[Int] = convertToIntArray(reader.next())
    val result: Int = calcNrSwapsInInsertionSort(array, n)
    println(result)
  }
}
