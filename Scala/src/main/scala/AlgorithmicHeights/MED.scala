package AlgorithmicHeights

object MED {
  import scala.annotation.tailrec
  import scala.util.Random

  private val rng = new Random(2112)

  private def convertToIntArray(line: String): Array[Int] = line.split(" ").map(_.toInt)

  private def swapElems(array: Array[Int], ix: Int, jy: Int): Unit = {
    if (ix != jy) {
      val elem: Int = array(ix)
      array(ix) = array(jy)
      array(jy) = elem
    }
  }

  private def threeWayPartitioning(array: Array[Int], pivot: Int, startIx: Int, endIx: Int): (Int, Int) = {
    @tailrec
    def loop(currentIx: Int, startIx: Int, endIx: Int): (Int, Int) = {
      if (currentIx > endIx) (startIx, endIx + 1)
      else if (array(currentIx) < pivot) {
        swapElems(array, currentIx, startIx)
        loop(currentIx + 1, startIx + 1, endIx)
      }
      else if (array(currentIx) > pivot) {
        swapElems(array, currentIx, endIx)
        loop(currentIx, startIx, endIx - 1)
      }
      else loop(currentIx + 1, startIx, endIx)
    }

    loop(currentIx = startIx, startIx = startIx, endIx = endIx)
  }

  def findKthSmallestElement(array: Array[Int], n: Int, k: Int): Int = {
    @tailrec
    def loop(startIx: Int, endIx: Int): Int = {
      val randomIx: Int = rng.between(startIx, endIx + 1)
      val pivot: Int = array(randomIx)
      val (middleStart, middleEnd): (Int, Int) = threeWayPartitioning(array, pivot, startIx, endIx)
      if (k <= middleStart) loop(startIx, middleStart)
      else if (middleStart < k && k <= middleEnd) array(middleStart)
      else loop(middleEnd, endIx)
    }

    loop(startIx = 0, endIx = n - 1)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val array: Array[Int] = convertToIntArray(reader.next())
    val k: Int = reader.next().toInt
    val result: Int = findKthSmallestElement(array, n, k)
    println(result)
  }
}
