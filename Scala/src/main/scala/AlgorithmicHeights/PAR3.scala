package AlgorithmicHeights

object PAR3 {
  import scala.annotation.tailrec

  private def convertToIntArray(line: String): Array[Int] = line.split(" ").map(_.toInt)

  private def swapElems(array: Array[Int], ix: Int, jy: Int): Unit = {
    val elem: Int = array(ix)
    array(ix) = array(jy)
    array(jy) = elem
  }

  def threeWayPartitioning(array: Array[Int], pivot: Int, n: Int): Unit = {
    @tailrec
    def loop(middleIx: Int, currentIx: Int, endIx: Int): Unit = {
      if (currentIx > endIx) ()
      else if (array(currentIx) < pivot) {
        swapElems(array, currentIx, middleIx)
        loop(middleIx + 1, currentIx + 1, endIx)
      }
      else if (array(currentIx) > pivot) {
        swapElems(array, currentIx, endIx)
        loop(middleIx, currentIx, endIx - 1)
      }
      else loop(middleIx, currentIx + 1, endIx)
    }

    loop(middleIx = 0, currentIx = 1, endIx = n - 1)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val array: Array[Int] = convertToIntArray(reader.next())
    threeWayPartitioning(array, array(0), n)
    println(array.mkString(" "))
  }
}
