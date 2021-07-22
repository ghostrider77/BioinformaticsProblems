package TextbookTrack.Chapter09

object BA9Q {
  import scala.annotation.tailrec
  import TextbookTrack.RichNumber._

  private def sortSingleCharacters(text: String): (Vector[Int], Vector[Int]) = {
    val indices: Map[Char, IndexedSeq[Int]] = text.zipWithIndex.groupMap{ case (char, _) => char }{ case (_, ix) => ix }
    val uniqueChars: Vector[Char] = indices.keys.toVector.sorted
    val charIds: Map[Char, Int] = uniqueChars.zipWithIndex.toMap
    val order: Vector[Int] = uniqueChars.flatMap(indices(_))
    val classes: Vector[Int] = text.map(charIds(_)).toVector
    (order, classes)
  }

  private def countCumulativeClassSizes(classes: Vector[Int], n: Int): Array[Int] = {
    val cumulativeCounts: Array[Int] = Array.fill(n)(0)
    val classSizes: Map[Int, Int] = classes.groupMapReduce(identity)(_ => 1)(_ + _)
    classSizes.foreach{ case (classId, classSize) => cumulativeCounts(classId) = classSize }
    (1 until n).foreach(ix => cumulativeCounts(ix) += cumulativeCounts(ix - 1))
    cumulativeCounts
  }

  private def sortDoubledShifts(order: Vector[Int], classes: Vector[Int], n: Int, cyclicShiftSize: Int): Vector[Int] = {
    val counts: Array[Int] = countCumulativeClassSizes(classes, n)
    val newOrder: Array[Int] = Array.fill(n)(0)
    for { ix <- (n - 1) to 0 by -1 } {
      val start: Int = (order(ix) - cyclicShiftSize) mod n
      val classId: Int = classes(start)
      counts(classId) -= 1
      newOrder(counts(classId)) = start
    }
    newOrder.toVector
  }

  private def updateClasses(order: Vector[Int], classes: Vector[Int], cyclicShiftSize: Int, n: Int): Vector[Int] = {
    val newClasses: Array[Int] = Array.fill(n)(0)
    newClasses(order(0)) = 0
    val orders: Iterator[(Int, Int)] = order.sliding(2).collect{ case Vector(a, b) => (a, b) }

    @tailrec
    def loop(label: Int): Unit = {
      if (orders.isEmpty) ()
      else {
        val (previous, current): (Int, Int) = orders.next()
        val mid: Int = (current + cyclicShiftSize) % n
        val midPrevious: Int = (previous + cyclicShiftSize) % n
        val nextLabel: Int =
          if (classes(current) != classes(previous) || classes(mid) != classes(midPrevious)) label + 1 else label
        newClasses(current) = nextLabel
        loop(nextLabel)
      }
    }

    loop(0)
    newClasses.toVector
  }

  private def computeFullSuffixArray(text: String): Vector[Int] = {
    val n: Int = text.length
    val (order, classes): (Vector[Int], Vector[Int]) = sortSingleCharacters(text)

    @tailrec
    def loop(cyclicShiftSize: Int, cyclicShiftOrders: Vector[Int], equivalenceClasses: Vector[Int]): Vector[Int] = {
      if (cyclicShiftSize >= n) cyclicShiftOrders
      else {
        val doubleShiftOrders: Vector[Int] =
          sortDoubledShifts(cyclicShiftOrders, equivalenceClasses, n, cyclicShiftSize)
        val updatedClasses: Vector[Int] = updateClasses(doubleShiftOrders, equivalenceClasses, cyclicShiftSize, n)
        loop(2 * cyclicShiftSize, doubleShiftOrders, updatedClasses)
      }
    }

    loop(1, order, classes)
  }

  def computePartialSuffixArray(text: String, k: Int): List[(Int, Int)] = {
    val suffixArray: Vector[Int] = computeFullSuffixArray(text)
    suffixArray
      .iterator
      .zipWithIndex
      .withFilter{ case (suffixIx, _) => suffixIx % k == 0 }
      .map(_.swap)
      .toList
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next()
    val k: Int = reader.next().toInt
    val result: List[(Int, Int)] = computePartialSuffixArray(text, k)
    result.foreach{ case (ix, suffixIx) => println(s"$ix,$suffixIx") }
  }
}
