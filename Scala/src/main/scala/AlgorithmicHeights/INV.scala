package AlgorithmicHeights

object INV {
  import scala.annotation.tailrec

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def mergeSortedLists(list1: List[Int], n: Int, list2: List[Int], inversions: Long): (List[Int], Long) = {
    @tailrec
    def loop(acc: List[Int], inv: Long, xs: List[Int], length: Int, ys: List[Int]): (List[Int], Long) = (xs, ys) match {
      case (Nil, Nil) => (acc.reverse, inv)
      case (x :: xss, Nil) => loop(x :: acc, inv, xss, length - 1, ys)
      case (Nil, y :: yss) => loop(y :: acc, inv, xs, length, yss)
      case (x :: xss, y :: yss) =>
        if (x <= y) loop(x :: acc, inv, xss, length - 1, ys)
        else loop(y :: acc, inv + length, xs, length, yss)
    }

    loop(Nil, inversions, list1, n, list2)
  }

  def countInversions(array: List[Int], n: Int): (List[Int], Long) = {
    if (n <= 1) (array, 0L)
    else {
      val k: Int = n / 2
      val (first, second): (List[Int], List[Int]) = array.splitAt(k)
      val (sortedFirst, inversionsInFirst): (List[Int], Long) = countInversions(first, k)
      val (sortedSecond, inversionsInSecond): (List[Int], Long) = countInversions(second, n - k)
      mergeSortedLists(sortedFirst, k, sortedSecond, inversionsInFirst + inversionsInSecond)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val array: List[Int] = convertToIntList(reader.next())
    val (_, result): (List[Int], Long) = countInversions(array, n)
    println(result)
  }
}
