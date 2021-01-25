package TextbookTrack.Chapter01

object BA1I {
  import scala.math.Integral.Implicits._

  private val Nucleotides: Vector[Char] = Vector('A', 'C', 'G', 'T')
  private val NucleotideOrder: Map[Char, Int] = Map('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(k, d) => (k, d)
    case _ => throw new Exception("Unexpected input data format.")
  }

  private def patternToNumber(pattern: String): Int =
    pattern.foldLeft(0)((acc, nucleotide) => 4 * acc + NucleotideOrder(nucleotide))

  private def numberToPattern(encoding: Int, k: Int): String = {
    val (pattern, _): (List[Char], Int) = (0 until k).foldLeft((List.empty[Char], encoding)) {
      case ((acc, code), _) =>
        val (nextCode, remainder): (Int, Int) = code /% 4
        (Nucleotides(remainder) :: acc, nextCode)
    }
    pattern.mkString
  }

  private def calcFrequenciesForApproximateOccurrences(text: String, k: Int, d: Int): Array[Int] = {
    val frequencies: Array[Int] = Array.fill(math.pow(4, k).toInt)(0)
    val kMerEncodings: Iterator[Int] = text.sliding(k).flatMap{ pattern =>
      val neighbours: Set[String] = generateNeighbourhood(pattern, d)
      neighbours.map(patternToNumber)
    }
    kMerEncodings.foreach(code => frequencies(code) += 1)
    frequencies
  }

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

  def mostFrequentApproximateKMers(text: String, k: Int, d: Int): List[String] = {
    val frequencies: Array[Int] = calcFrequenciesForApproximateOccurrences(text, k, d)
    val maxOccurrence: Int = frequencies.max
    frequencies
      .iterator
      .zipWithIndex
      .filter{ case (freq, _) => freq == maxOccurrence }
      .map{ case (_, ix) => numberToPattern(ix, k) }
      .toList
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next()
    val (k, d): (Int, Int) = readParameters(reader.next())
    val result: List[String] = mostFrequentApproximateKMers(text, k, d)
    println(result.mkString(" "))
  }
}
