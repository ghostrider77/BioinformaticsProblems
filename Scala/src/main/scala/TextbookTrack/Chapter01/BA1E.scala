package TextbookTrack.Chapter01

object BA1E {
  import scala.math.Integral.Implicits._

  private val Nucleotides: Vector[Char] = Vector('A', 'C', 'G', 'T')
  private val NucleotideOrder: Map[Char, Int] = Map('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int, Int) = convertToIntList(line) match {
    case List(k, l, t) => (k, l, t)
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

  private def computeFrequencies(text: String, k: Int): Array[Int] = {
    val frequencies: Array[Int] = Array.fill(math.pow(4, k).toInt)(0)
    val kMerEncodings: Iterator[Int] = text.sliding(k).map(patternToNumber)
    kMerEncodings.foreach(code => frequencies(code) += 1)
    frequencies
  }

  private def collectClumpFormingKMers(clumpArray: Array[Boolean], k: Int): List[String] =
    clumpArray
      .iterator
      .zipWithIndex
      .filter{ case (isClump, _) => isClump }
      .map{ case (_, ix) => numberToPattern(ix, k) }
      .toList

  def findClumpsInText(genome: String, k: Int, l: Int, t: Int): List[String] = {
    val clumpArray: Array[Boolean] = Array.fill(math.pow(4, k).toInt)(false)
    val frequencies: Array[Int] = computeFrequencies(genome.take(l), k)

    for {
      (freq, ix) <- frequencies.iterator.zipWithIndex
      if freq >= t
    } clumpArray(ix) = true

    (1 to (genome.length - l)).foreach { ix =>
      val startKMer: String = genome.slice(ix - 1, ix - 1 + k)
      val startCode: Int = patternToNumber(startKMer)
      frequencies(startCode) -= 1
      val endKMer: String = genome.slice(ix + l - k, ix + l)
      val endCode: Int = patternToNumber(endKMer)
      frequencies(endCode) += 1
      if (frequencies(endCode) >= t) clumpArray(endCode) = true
    }

    collectClumpFormingKMers(clumpArray, k)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val genome: String = reader.next()
    val (k, l, t): (Int, Int, Int) = readParameters(reader.next())
    val result: List[String] = findClumpsInText(genome, k, l, t)
    println(result.mkString(" "))
  }
}
