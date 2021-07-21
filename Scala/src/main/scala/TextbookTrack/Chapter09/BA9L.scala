package TextbookTrack.Chapter09

object BA9L {
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

  private def findPatternInText(pattern: String,
                                firstColumn: Map[CharacterIndex, Int],
                                lastColumn: Vector[CharacterIndex]): Int = {
    val reversedPattern: Iterator[Char] = pattern.reverseIterator

    @tailrec
    def loop(topPointer: Int, bottomPointer: Int): Int =
      reversedPattern.nextOption() match {
        case None => bottomPointer - topPointer + 1
        case Some(letter) =>
          val letterIndices: Vector[(CharacterIndex, Int)] =
            lastColumn.slice(topPointer, bottomPointer + 1).zip(topPointer to bottomPointer)
          val topLetterIndex: Option[Int] =
            letterIndices.find{ case (CharacterIndex(char, _), _) => char == letter }.map{ case (_, ix) => ix }
          val bottomLetterIndex: Option[Int] =
            letterIndices.findLast{ case (CharacterIndex(char, _), _) => char == letter }.map{ case (_, ix) => ix }
          (topLetterIndex, bottomLetterIndex) match {
            case (Some(topIx), Some(bottomIx)) =>
              loop(firstColumn(lastColumn(topIx)), firstColumn(lastColumn(bottomIx)))
            case _ => 0
          }
      }

    loop(0, lastColumn.length - 1)
  }

  def performPatternMatching(transformedText: List[Char], patterns: List[String]): List[Int] = {
    val lastColumn: Vector[CharacterIndex] = createIndexedColumn(transformedText)
    val firstColumn: Map[CharacterIndex, Int] = createIndexedColumn(transformedText.sorted).zipWithIndex.toMap
    patterns.map(findPatternInText(_, firstColumn, lastColumn))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val transformedText: List[Char] = reader.next().toList
    val patterns: List[String] = reader.next().split(" ").toList
    val result: List[Int] = performPatternMatching(transformedText, patterns)
    println(result.mkString(" "))
  }
}
