package TextbookTrack.Chapter03

object BA3L {
  final case class GappedPattern(first: String, second: String) {
    def prefix: GappedPattern = GappedPattern(first.dropRight(1), second.dropRight(1))

    def suffix: GappedPattern = GappedPattern(first.drop(1), second.drop(1))
  }

  private def readParameters(line: String): (Int, Int) = line.split(" ").map(_.toInt).toList match {
    case List(k, d) => (k, d)
    case _ => throw new Exception("Unexpected input data format.")
  }

  private def readGappedPatterns(reader: Iterator[String]): List[GappedPattern] =
    reader.map{ line =>
      line.split("\\|").toList match {
        case List(first, second) => GappedPattern(first, second)
        case _ => throw new Exception("Unexpected input data format.")
      }
    }.toList

  def calcStringSpelledByAGappedGenomePath(pairedPatterns: List[GappedPattern], k: Int, d: Int): Option[String] = {
    def calcStringSpelledByAGenomePath(kMers: List[String]): String = kMers match {
      case Nil => ""
      case x :: xs => (x.toList ::: xs.map(_.last)).mkString
    }

    val firstGenome: String = calcStringSpelledByAGenomePath(pairedPatterns.map(_.first))
    val secondGenome: String = calcStringSpelledByAGenomePath(pairedPatterns.map(_.second))
    if (firstGenome.drop(k + d) != secondGenome.dropRight(k + d)) None
    else Some(firstGenome.take(k + d) + secondGenome)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (k, d): (Int, Int) = readParameters(reader.next())
    val pairedPatterns: List[GappedPattern] = readGappedPatterns(reader)
    calcStringSpelledByAGappedGenomePath(pairedPatterns, k, d) match {
      case None => println("No string spelled by the gapped pattern")
      case Some(genome) => println(genome)
    }
  }
}
