package TextbookTrack.Chapter01

object BA1N {
  private val Nucleotides: Vector[Char] = Vector('A', 'C', 'G', 'T')

  private def calcImmediateNeighbours(pattern: String): Set[String] =
    pattern.view.zipWithIndex.foldLeft(Set.empty[String]){
      case (neighbours, (letter, ix)) =>
        neighbours ++ Nucleotides.filterNot(_ == letter).map(nucleotide => pattern.updated(ix, nucleotide)).toSet
    }

  def generateNeighbourhood(text: String, d: Int): Set[String] =
    (0 until d).foldLeft(Set(text)) {
      case (neighbourhood, _) => neighbourhood ++ neighbourhood.flatMap(calcImmediateNeighbours)
    }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val pattern: String = reader.next()
    val d: Int = reader.next().toInt
    val result: Set[String] = generateNeighbourhood(pattern, d)
    result.foreach(println)
  }
}
