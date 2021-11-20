package AlgorithmicHeights

object PAR {
  import scala.annotation.tailrec

  private def convertToIntArray(line: String): Array[Int] = line.split(" ").map(_.toInt)

  private def swapElems(array: Array[Int], ix: Int, jy: Int): Unit = {
    val elem: Int = array(ix)
    array(ix) = array(jy)
    array(jy) = elem
  }

  def twoWayPartitioning(array: Array[Int], pivot: Int, n: Int): Unit = {
    @tailrec
    def loop(startIx: Int, endIx: Int): Unit = {
      if (startIx > endIx) ()
      else if (array(startIx) <= pivot) {
        swapElems(array, startIx, startIx - 1)
        loop(startIx + 1, endIx)
       }
      else {
        swapElems(array, startIx, endIx)
        loop(startIx, endIx - 1)
      }
    }

    loop(1, n - 1)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val array: Array[Int] = convertToIntArray(reader.next())
    twoWayPartitioning(array, array(0), n)
    println(array.mkString(" "))
  }
}
