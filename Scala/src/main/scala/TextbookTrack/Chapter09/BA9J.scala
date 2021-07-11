package TextbookTrack.Chapter09

object BA9J {
  import scala.annotation.tailrec

  final case class CharacterIndex(char: Char, number: Int)

  private def createIndexedColumn(string: List[Char]): Vector[CharacterIndex] = {
    @tailrec
    def loop(counts: Map[Char, Int],
             charIndex: List[CharacterIndex],
             xs: List[Char]): Vector[CharacterIndex] = xs match {
      case Nil => charIndex.reverseIterator.toVector
      case x :: xss =>
        val count: Int = counts.getOrElse(x, 0)
        loop(counts.updated(x, count + 1), CharacterIndex(x, count) :: charIndex, xss)
    }

    loop(Map(), Nil, string)
  }

  def calcInverseBurrowsWheelerTransform(transformedText: List[Char]): String = {
    val n: Int = transformedText.length
    val lastColumn: Vector[CharacterIndex] = createIndexedColumn(transformedText)
    val firstColumn: Map[CharacterIndex, Int] = createIndexedColumn(transformedText.sorted).zipWithIndex.toMap

    val (string, _): (List[Char], Int) =
      (0 until (n - 1)).foldLeft((List('$'), 0)){
        case ((acc, position), _) =>
          val letterCount: CharacterIndex = lastColumn(position)
          (letterCount.char :: acc, firstColumn(letterCount))
      }
    string.mkString
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val transformedText: List[Char] = reader.next().toList
    val result: String = calcInverseBurrowsWheelerTransform(transformedText)
    println(result)
  }
}
