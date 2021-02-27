package TextbookTrack.Chapter05

object BA5C {
  import scala.annotation.tailrec

  private def collectLCS(backtrack: Array[Array[Byte]], string: String, n1: Int, n2: Int): String = {
    @tailrec
    def loop(lcs: List[Char], ix: Int, jy: Int): String = {
      if (ix == 0 || jy == 0) lcs.mkString
      else if (backtrack(ix - 1)(jy - 1) == 0) loop(string(ix - 1) :: lcs, ix - 1, jy - 1)
      else if (backtrack(ix - 1)(jy - 1) == -1) loop(lcs, ix - 1, jy)
      else loop(lcs, ix, jy - 1)
    }

    loop(Nil, n1, n2)
  }

  def calcLongestCommonSubsequence(string1: String, string2: String): String = {
    val n1: Int = string1.length
    val n2: Int = string2.length
    val table: Array[Array[Int]] = Array.fill(n1 + 1, n2 + 1)(0)
    val backtrack: Array[Array[Byte]] = Array.fill(n1, n2)(0)

    for {
      (c1, ix) <- string1.zipWithIndex
      (c2, jy) <- string2.zipWithIndex
    } {
      val pathDown: Int = table(ix)(jy + 1)
      val pathRight: Int = table(ix + 1)(jy)
      val pathDiag: Int = table(ix)(jy) + (if (c1 == c2) 1 else 0)
      val maxPath: Int = List(pathDown, pathRight, pathDiag).max
      table(ix + 1)(jy + 1) = maxPath
      backtrack(ix)(jy) = if (maxPath == pathDown) -1 else if (maxPath == pathRight) 1 else 0
    }

    collectLCS(backtrack, string1, n1, n2)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val string1: String = reader.next()
    val string2: String = reader.next()
    val result: String = calcLongestCommonSubsequence(string1, string2)
    println(result)
  }
}
