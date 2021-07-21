package TextbookTrack.Chapter09

object BA9K {
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

  def calcLastToFirstMapping(transformedText: List[Char], ix: Int): Int = {
    val lastColumn: Vector[CharacterIndex] = createIndexedColumn(transformedText)
    val firstColumn: Map[CharacterIndex, Int] = createIndexedColumn(transformedText.sorted).zipWithIndex.toMap
    firstColumn(lastColumn(ix))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val transformedText: List[Char] = reader.next().toList
    val ix: Int = reader.next().toInt
    val result: Int = calcLastToFirstMapping(transformedText, ix)
    println(result)
  }
}
