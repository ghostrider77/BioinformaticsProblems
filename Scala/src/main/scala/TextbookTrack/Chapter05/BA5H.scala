package TextbookTrack.Chapter05

object BA5H {
  import scala.annotation.tailrec

  private def collectFittingAlignment(backtrack: Array[Array[Byte]],
                                      string1: String,
                                      string2: String,
                                      maxRowIx: Int): (String, String) = {
    @tailrec
    def loop(aligned: List[(Char, Char)], ix: Int, jy: Int): (String, String) = {
      if (ix == 0 && jy == 0) {
        val (aligned1, aligned2): (List[Char], List[Char]) = aligned.unzip
        (aligned1.mkString, aligned2.mkString)
      }
      else if (ix == 0) loop(('-', string2(jy - 1)) :: aligned, ix, jy - 1)
      else if (jy == 0) loop(aligned, 0, jy)
      else backtrack(ix - 1)(jy - 1) match {
        case 0 => loop((string1(ix - 1), '-') :: aligned, ix - 1, jy)
        case 1 => loop(('-', string2(jy - 1)) :: aligned, ix, jy - 1)
        case _ => loop((string1(ix - 1), string2(jy - 1)) :: aligned, ix - 1, jy - 1)
      }
    }

    loop(Nil, maxRowIx, string2.length)
  }

  def calcFittingAlignment(string1: String, string2: String): (Int, String, String) = {
    val n: Int = string1.length
    val m: Int = string2.length
    val table: Array[Array[Int]] = Array.fill(n + 1, m + 1)(0)
    val backtrack: Array[Array[Byte]] = Array.fill(n, m)(0)
    (1 to m).foreach(jy => table(0)(jy) = -jy)

    for {
      (c1, ix) <- string1.zipWithIndex
      (c2, jy) <- string2.zipWithIndex
    } {
      val deletion: Int = table(ix)(jy + 1) - 1
      val insertion: Int = table(ix + 1)(jy) - 1
      val matching: Int = table(ix)(jy) + (if (c1 == c2) 1 else -1)
      val (maxPath, maxIx): (Int, Int) =
        Iterator(deletion, insertion, matching).zipWithIndex.maxBy{ case (score, _) => score }
      table(ix + 1)(jy + 1) = maxPath
      backtrack(ix)(jy) = maxIx.toByte
    }
    val (maxValue, maxRowIx): (Int, Int) = table.map(_.last).zipWithIndex.maxBy{ case (score, _) => score }
    table(n)(m) = maxValue
    val (alignedString1, alignedString2): (String, String) =
      collectFittingAlignment(backtrack, string1, string2, maxRowIx)
    (table(n)(m), alignedString1, alignedString2)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val string1: String = reader.next()
    val string2: String = reader.next()
    val (score, alignedString1, alignedString2): (Int, String, String) = calcFittingAlignment(string1, string2)
    println(score)
    println(alignedString1)
    println(alignedString2)
  }
}
