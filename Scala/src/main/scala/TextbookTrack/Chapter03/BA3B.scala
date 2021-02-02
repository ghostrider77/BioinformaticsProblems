package TextbookTrack.Chapter03

object BA3B {
  def calcStringSpelledByAGenomePath(kMers: List[String]): String = kMers match {
    case Nil => ""
    case x :: xs => (x.toList ::: xs.map(_.last)).mkString
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val kMers: List[String] = reader.toList
    val result: String = calcStringSpelledByAGenomePath(kMers)
    println(result)
  }
}
