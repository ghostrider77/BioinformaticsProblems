package TextbookTrack.Chapter05

object BA5E {
  import scala.annotation.tailrec
  import TextbookTrack.Utils.readScoringMatrix

  private val Blosum62: Map[(Char, Char), Int] = readScoringMatrix("BLOSUM62")

  private def collectGlobalAlignment(backtrack: Array[Array[Byte]],
                                     string1: String,
                                     string2: String): (String, String) = {
    @tailrec
    def loop(aligned: List[(Char, Char)], ix: Int, jy: Int): (String, String) = {
      if (ix == 0 && jy == 0) {
        val (aligned1, aligned2): (List[Char], List[Char]) = aligned.unzip
        (aligned1.mkString, aligned2.mkString)
      }
      else if (ix == 0) loop(('-', string2(jy - 1)) :: aligned, ix, jy - 1)
      else if (jy == 0) loop((string1(ix - 1), '-') :: aligned, ix - 1, jy)
      else if (backtrack(ix - 1)(jy - 1) == 0) loop((string1(ix - 1), string2(jy - 1)) :: aligned, ix - 1, jy - 1)
      else if (backtrack(ix - 1)(jy - 1) == -1) loop((string1(ix - 1), '-') :: aligned, ix - 1, jy)
      else loop(('-', string2(jy - 1)) :: aligned, ix, jy - 1)
    }

    loop(Nil, string1.length, string2.length)
  }

  def calcGlobalAlignment(string1: String, string2: String, sigma: Int): (Int, String, String) = {
    val n: Int = string1.length
    val m: Int = string2.length
    val table: Array[Array[Int]] = Array.fill(n + 1, m + 1)(0)
    val backtrack: Array[Array[Byte]] = Array.fill(n, m)(0)
    (1 to n).foreach(ix => table(ix)(0) = -ix * sigma)
    (1 to m).foreach(jy => table(0)(jy) = -jy * sigma)

    for {
      (c1, ix) <- string1.zipWithIndex
      (c2, jy) <- string2.zipWithIndex
    } {
      val deletion: Int = table(ix)(jy + 1) - sigma
      val insertion: Int = table(ix + 1)(jy) - sigma
      val matching: Int = table(ix)(jy) + Blosum62((c1, c2))
      val maxPath: Int = List(deletion, insertion, matching).max
      table(ix + 1)(jy + 1) = maxPath
      backtrack(ix)(jy) = if (maxPath == deletion) -1 else if (maxPath == insertion) 1 else 0
    }

    val (alignedString1, alignedString2): (String, String) = collectGlobalAlignment(backtrack, string1, string2)
    (table(n)(m), alignedString1, alignedString2)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val string1: String = reader.next()
    val string2: String = reader.next()
    val sigma: Int = 5
    val (score, alignedString1, alignedString2): (Int, String, String) = calcGlobalAlignment(string1, string2, sigma)
    println(score)
    println(alignedString1)
    println(alignedString2)
  }
}
