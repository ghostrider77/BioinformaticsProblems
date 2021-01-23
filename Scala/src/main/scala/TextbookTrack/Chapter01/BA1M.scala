package TextbookTrack.Chapter01

object BA1M {

  import scala.math.Integral.Implicits._

  private val Nucleotides: Vector[Char] = Vector('A', 'C', 'G', 'T')

  def numberToPattern(encoding: Int, k: Int): String = {
    val (pattern, _): (List[Char], Int) = (0 until k).foldLeft((List.empty[Char], encoding)) {
      case ((acc, code), _) =>
        val (nextCode, remainder): (Int, Int) = code /% 4
        (Nucleotides(remainder) :: acc, nextCode)
    }
    pattern.mkString
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val encoding: Int = reader.next().toInt
    val k: Int = reader.next().toInt
    val result: String = numberToPattern(encoding, k)
    println(result)
  }
}
