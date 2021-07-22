package TextbookTrack.Chapter09

object BA9M {
  import scala.annotation.tailrec

  private def calcFirstOccurrencePositions(text: List[Char]): Map[Char, Int] = {
    val letterCounts: Map[Char, Int] = text.groupMapReduce(identity)(_ => 1)(_ + _)
    letterCounts
      .keysIterator
      .toList
      .sorted
      .foldLeft((Map.empty[Char, Int], 0)) {
        case ((firstOccurrence, ix), letter) => (firstOccurrence.updated(letter, ix), ix + letterCounts(letter))
      }
      ._1
  }

  private def calcCountMatrix(transformedText: List[Char], n: Int, uniqueLetters: Set[Char]): Map[Char, Array[Int]] = {
    val countMatrix: Map[Char, Array[Int]] = uniqueLetters.map(_ -> Array.fill(n + 1)(0)).toMap
    for {
      (letter, ix) <- transformedText.view.zipWithIndex
      char <- uniqueLetters
    } {
      val counts: Array[Int] = countMatrix(char)
      counts(ix + 1) = if (letter == char) counts(ix) + 1 else counts(ix)
    }

    countMatrix
  }

  private def patternMatching(pattern: String,
                              firstOccurrences: Map[Char, Int],
                              countMatrix: Map[Char, Array[Int]],
                              n: Int): Int = {
    val reversedPattern: Iterator[Char] = pattern.reverseIterator

    @tailrec
    def loop(topPointer: Int, bottomPointer: Int): Int = reversedPattern.nextOption() match {
      case None => bottomPointer - topPointer + 1
      case Some(letter) =>
        val updatedPointers: Option[(Int, Int)] =
          firstOccurrences
            .get(letter)
            .map{ letterOccurrence =>
              val letterCounter: Array[Int] = countMatrix(letter)
              (letterOccurrence + letterCounter(topPointer), letterOccurrence + letterCounter(bottomPointer + 1) - 1)
            }
        updatedPointers match {
          case None => 0
          case Some((top, bottom)) => if (top > bottom) 0 else loop(top, bottom)
        }
    }

    loop(0, n - 1)
  }

  def performPatternMatching(transformedText: List[Char], patterns: List[String]): List[Int] = {
    val length: Int = transformedText.length
    val firstOccurrences: Map[Char, Int] = calcFirstOccurrencePositions(transformedText)
    val uniqueLetters: Set[Char] = firstOccurrences.keySet
    val countMatrix: Map[Char, Array[Int]] = calcCountMatrix(transformedText, length, uniqueLetters)
    patterns.map(patternMatching(_, firstOccurrences, countMatrix, length))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val transformedText: List[Char] = reader.next().toList
    val patterns: List[String] = reader.next().split(" ").toList
    val result: List[Int] = performPatternMatching(transformedText, patterns)
    println(result.mkString(" "))
  }
}
