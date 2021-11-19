package AlgorithmicHeights

object MS {
  import scala.annotation.tailrec

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def mergeSortedLists(list1: List[Int], list2: List[Int]): List[Int] = {
    @tailrec
    def loop(acc: List[Int], xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, Nil) => acc.reverse
      case (x :: xss, Nil) => loop(x :: acc, xss, ys)
      case (Nil, y :: yss) => loop(y :: acc, xs, yss)
      case (x :: xss, y :: yss) => if (x <= y) loop(x :: acc, xss, ys) else loop(y :: acc, xs, yss)
    }

    loop(Nil, list1, list2)
  }

  def mergeSort(array: List[Int], n: Int): List[Int] = {
    if (n <= 1) array
    else {
      val k: Int = n / 2
      val (first, second): (List[Int], List[Int]) = array.splitAt(k)
      val sortedFirst: List[Int] = mergeSort(first, k)
      val sortedSecond: List[Int] = mergeSort(second, n - k)
      mergeSortedLists(sortedFirst, sortedSecond)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val array: List[Int] = convertToIntList(reader.next())
    val result: List[Int] = mergeSort(array, n)
    println(result.mkString(" "))
  }
}
