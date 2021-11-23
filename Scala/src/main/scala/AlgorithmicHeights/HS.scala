package AlgorithmicHeights

object HS {
  import scala.annotation.tailrec

  private def convertToIntArray(line: String): Array[Int] = line.split(" ").map(_.toInt)

  private def getIndexOfParentChildrenMaximum(array: Array[Int], parentIx: Int, size: Int): Int = {
    val leftChildIx: Int = 2 * parentIx + 1
    val rightChildIx: Int = leftChildIx + 1
    val maxIndex: Int = if (leftChildIx < size && array(leftChildIx) > array(parentIx)) leftChildIx else parentIx
    if (rightChildIx < size && array(rightChildIx) > array(maxIndex)) rightChildIx else maxIndex
  }

  private def swapElems(array: Array[Int], ix: Int, jy: Int): Unit = {
    val elem: Int = array(ix)
    array(ix) = array(jy)
    array(jy) = elem
  }

  private def siftDown(array: Array[Int], parentIx: Int, n: Int): Unit = {
    @tailrec
    def loop(currentParentIx: Int): Unit = {
      val maxIx: Int = getIndexOfParentChildrenMaximum(array, currentParentIx, n)
      if (maxIx == currentParentIx) ()
      else {
        swapElems(array, maxIx, currentParentIx)
        loop(maxIx)
      }
    }

    loop(parentIx)
  }

  private def heapify(array: Array[Int], n: Int): Unit =
    (n / 2 - 1 to 0 by -1).foreach(parentIx => siftDown(array, parentIx, n))

  def heapsort(array: Array[Int], size: Int): Unit = {
    heapify(array, size)
    (size until 1 by -1).foreach{
      n =>
        swapElems(array, 0, n - 1)
        siftDown(array, 0, n - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val array: Array[Int] = convertToIntArray(reader.next())
    heapify(array, n)
    println(array.mkString(" "))
  }
}
