package Stronghold

object HAMM {
  def calcHammingDistance(s1: String, s2: String): Int =
    s1.lazyZip(s2).count{ case (c1, c2) => c1 != c2 }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val s1: String = reader.next()
    val s2: String = reader.next()
    val result: Int = calcHammingDistance(s1, s2)
    println(result)
  }
}
