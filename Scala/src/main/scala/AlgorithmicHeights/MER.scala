package AlgorithmicHeights

object MER {
  import scala.annotation.tailrec

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def mergeSortedLists(list1: List[Int], list2: List[Int]): List[Int] = {
    @tailrec
    def loop(acc: List[Int], xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, Nil) => acc.reverse
      case (x :: xss, Nil) => loop(x :: acc, xss, ys)
      case (Nil, y :: yss) => loop(y :: acc, xs, yss)
      case (x :: xss, y :: yss) => if (x <= y) loop(x :: acc, xss, ys) else loop(y :: acc, xs, yss)
    }

    loop(Nil, list1, list2)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val _: Int = reader.next().toInt
    val list1: List[Int] = convertToIntList(reader.next())
    val _: Int = reader.next().toInt
    val list2: List[Int] = convertToIntList(reader.next())
    val result: List[Int] = mergeSortedLists(list1, list2)
    println(result.mkString(" "))
  }
}
