package TextbookTrack.Chapter05

object BA5F {
  import scala.annotation.tailrec
  import TextbookTrack.Utils.readScoringMatrix

  private val Pam250: Map[(Char, Char), Int] = readScoringMatrix("PAM250")

  private def collectLocalAlignment(backtrack: Array[Array[Byte]],
                                    string1: String,
                                    string2: String,
                                    maxRowIx: Int,
                                    maxColIx: Int): (String, String) = {
    @tailrec
    def loop(aligned: List[(Char, Char)], ix: Int, jy: Int): (String, String) = {
      if (ix == 0 && jy == 0) {
        val (aligned1, aligned2): (List[Char], List[Char]) = aligned.unzip
        (aligned1.mkString, aligned2.mkString)
      }
      else if (ix == 0) loop(('-', string2(jy - 1)) :: aligned, ix, jy - 1)
      else if (jy == 0) loop((string1(ix - 1), '-') :: aligned, ix - 1, jy)
      else backtrack(ix - 1)(jy - 1) match {
        case 0 => loop((string1(ix - 1), '-') :: aligned, ix - 1, jy)
        case 1 => loop(('-', string2(jy - 1)) :: aligned, ix, jy - 1)
        case 2 => loop((string1(ix - 1), string2(jy - 1)) :: aligned, ix - 1, jy - 1)
        case _ => loop(aligned, 0, 0)
      }
    }

    loop(Nil, maxRowIx, maxColIx)
  }

  private def calcMaximumInTable(table: Array[Array[Int]]): (Int, Int, Int) =
    table.iterator.zipWithIndex.foldLeft((0, 0, 0)){
      case (acc @ (maxValue, _, _), (row, ix)) =>
        val (maxValueInRow, maxIx): (Int, Int) = row.iterator.zipWithIndex.maxBy{ case (score, _) => score }
        if (maxValueInRow > maxValue) (maxValueInRow, ix, maxIx)
        else acc
    }

  def calcLocalAlignment(string1: String, string2: String, sigma: Int): (Int, String, String) = {
    val n: Int = string1.length
    val m: Int = string2.length
    val table: Array[Array[Int]] = Array.fill(n + 1, m + 1)(0)
    val backtrack: Array[Array[Byte]] = Array.fill(n, m)(0)

    for {
      (c1, ix) <- string1.zipWithIndex
      (c2, jy) <- string2.zipWithIndex
    } {
      val deletion: Int = table(ix)(jy + 1) - sigma
      val insertion: Int = table(ix + 1)(jy) - sigma
      val matching: Int = table(ix)(jy) + Pam250((c1, c2))
      val (maxPath, maxIx): (Int, Int) =
        Iterator(deletion, insertion, matching, 0).zipWithIndex.maxBy{ case (score, _) => score }
      table(ix + 1)(jy + 1) = maxPath
      backtrack(ix)(jy) = maxIx.toByte
    }

    val (maxValue, maxRowIx, maxColIx): (Int, Int, Int) = calcMaximumInTable(table)
    table(n)(m) = maxValue
    val (alignedString1, alignedString2): (String, String) =
      collectLocalAlignment(backtrack, string1, string2, maxRowIx, maxColIx)
    (table(n)(m), alignedString1, alignedString2)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val string1: String = reader.next()
    val string2: String = reader.next()
    val sigma: Int = 5
    val (score, alignedString1, alignedString2): (Int, String, String) = calcLocalAlignment(string1, string2, sigma)
    println(score)
    println(alignedString1)
    println(alignedString2)
  }
}
