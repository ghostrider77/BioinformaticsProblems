package AlgorithmicHeights

object PS {
  import scala.annotation.tailrec

  private def convertToIntArray(line: String): Array[Int] = line.split(" ").map(_.toInt)

  private def getIndexOfParentChildrenMinimum(array: Array[Int], parentIx: Int, size: Int): Int = {
    val leftChildIx: Int = 2 * parentIx + 1
    val rightChildIx: Int = leftChildIx + 1
    val minIndex: Int = if (leftChildIx < size && array(leftChildIx) < array(parentIx)) leftChildIx else parentIx
    if (rightChildIx < size && array(rightChildIx) < array(minIndex)) rightChildIx else minIndex
  }

  private def swapElems(array: Array[Int], ix: Int, jy: Int): Unit = {
    val elem: Int = array(ix)
    array(ix) = array(jy)
    array(jy) = elem
  }

  private def siftDown(array: Array[Int], parentIx: Int, n: Int): Unit = {
    @tailrec
    def loop(currentParentIx: Int): Unit = {
      val minIx: Int = getIndexOfParentChildrenMinimum(array, currentParentIx, n)
      if (minIx == currentParentIx) ()
      else {
        swapElems(array, minIx, currentParentIx)
        loop(minIx)
      }
    }

    loop(parentIx)
  }

  private def heapify(array: Array[Int], n: Int): Unit =
    (n / 2 - 1 to 0 by -1).foreach(parentIx => siftDown(array, parentIx, n))

  def partialSort(array: Array[Int], size: Int, k: Int): Vector[Int] = {
    heapify(array, size)
    (size until (size - k) by -1).foreach{
      n =>
        swapElems(array, 0, n - 1)
        siftDown(array, 0, n - 1)
    }
    array.takeRight(k).reverseIterator.toVector
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val array: Array[Int] = convertToIntArray(reader.next())
    val k: Int = reader.next().toInt
    val result: Vector[Int] = partialSort(array, n, k)
    println(result.mkString(" "))
  }
}
