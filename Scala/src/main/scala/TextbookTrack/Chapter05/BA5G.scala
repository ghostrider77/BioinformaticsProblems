package TextbookTrack.Chapter05

object BA5G {
  def calcEditDistance(string1: String, string2: String): Int = {
    val n: Int = string1.length
    val m: Int = string2.length
    val table: Array[Array[Int]] = Array.fill(n + 1, m + 1)(0)
    (1 to n).foreach(ix => table(ix)(0) = ix)
    (1 to m).foreach(jy => table(0)(jy) = jy)

    for {
      (c1, ix) <- string1.zipWithIndex
      (c2, jy) <- string2.zipWithIndex
    } {
      val deletion: Int = table(ix)(jy + 1) + 1
      val insertion: Int = table(ix + 1)(jy) + 1
      val mismatch: Int = table(ix)(jy) + (if (c1 != c2) 1 else 0)
      table(ix + 1)(jy + 1) = Iterator(deletion, insertion, mismatch).min
    }
    table(n)(m)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val string1: String = reader.next()
    val string2: String = reader.next()
    val result: Int = calcEditDistance(string1, string2)
    println(result)
  }
}
