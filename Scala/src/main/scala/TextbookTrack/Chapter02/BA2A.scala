package TextbookTrack.Chapter02

object BA2A {
  private val Nucleotides: Vector[Char] = Vector('A', 'C', 'G', 'T')

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(k, d) => (k, d)
    case _ => throw new Exception("Unexpected input data format.")
  }

  private def calcHammingDistance(s1: String, s2: String): Int = s1.lazyZip(s2).count { case (c1, c2) => c1 != c2 }

  private def generateNeighbourhood(text: String, d: Int): Set[String] = {
    def calcImmediateNeighbours(pattern: String): Set[String] =
      pattern.view.zipWithIndex.foldLeft(Set.empty[String]){
        case (neighbours, (letter, ix)) =>
          neighbours ++ Nucleotides.filterNot(_ == letter).map(nucleotide => pattern.updated(ix, nucleotide)).toSet
      }

    (0 until d).foldLeft(Set(text)) {
      case (neighbourhood, _) => neighbourhood ++ neighbourhood.flatMap(calcImmediateNeighbours)
    }
  }

  def motifEnumeration(texts: Set[String], k: Int, d: Int): Set[String] = {
    def doesKMerOccursApproximatelyInText(text: String, kMer: String): Boolean =
      text.sliding(k).exists(calcHammingDistance(_, kMer) <= d)

    def allTextsApproximatelyContainPattern(kMer: String): Boolean =
      texts.forall(doesKMerOccursApproximatelyInText(_, kMer))

    for {
      text <- texts
      pattern <- text.sliding(k)
      neighbour <- generateNeighbourhood(pattern, d)
      if allTextsApproximatelyContainPattern(neighbour)
    } yield neighbour
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (k, d): (Int, Int) = readParameters(reader.next())
    val texts: Set[String] = reader.toSet
    val result: Set[String] = motifEnumeration(texts, k, d)
    println(result.mkString(" "))
  }
}
