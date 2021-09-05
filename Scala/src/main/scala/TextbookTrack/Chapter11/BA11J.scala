package TextbookTrack.Chapter11

object BA11J {
  import scala.annotation.tailrec
  import TextbookTrack.Utils.IntegerMassTable
  import scala.collection.mutable.{Map => MutableMap}

  class Matrix(val n: Int, val m: Int, val nrLayers: Int, initialValue: Double) {
    private val matrix: Array[Array[Array[Double]]] = Array.fill(n, m, nrLayers)(initialValue)

    def apply(i: Int, j: Int, k: Int): Double = matrix(i)(j)(k)

    def update(i: Int, j: Int, k: Int, value: Double): Unit = matrix(i)(j)(k) = value

    def argmax(): Int = {
      val (_, maxIndex): (Double, Int) = matrix.last.last.zipWithIndex.maxBy{ case (score, _) => score }
      maxIndex
    }
  }

  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  private def getPredecessorsOfANode(ix: Int, jy: Int, t: Int, diff: Vector[Int]): List[(Int, Int, Int)] = {
    val predecessorsAtDifferentLayers: List[(Int, Int, Int)] = (0 until jy).map((ix - 1, _, t - 1)).toList
    if (jy - diff(ix - 1) >= 0) (ix - 1, jy - diff(ix - 1), t) :: predecessorsAtDifferentLayers
    else predecessorsAtDifferentLayers
  }

  private def calcHighestScoringPeptideWithModifications(backtrack: MutableMap[(Int, Int, Int), (Int, Int, Int)],
                                                         maximalIndex: (Int, Int, Int),
                                                         peptide: String,
                                                         diff: Vector[Int]): String = {
    @tailrec
    def loop(modifiedPeptide: List[String], currentIndex: (Int, Int, Int), n: Int): String = {
      if (n == 0) modifiedPeptide.mkString
      else {
        val previousIndex: (Int, Int, Int) = backtrack(currentIndex)
        val modification: Int = currentIndex._2 - previousIndex._2 - diff(n - 1)
        val nextAminoACid: String = peptide(n - 1).toString
        val nextPart: String = {
          if (modification == 0) nextAminoACid
          else if (modification > 0) s"$nextAminoACid(+$modification)"
          else s"$nextAminoACid($modification)"
        }
        loop(nextPart :: modifiedPeptide, previousIndex, n - 1)
      }
    }

    loop(Nil, maximalIndex, peptide.length)
  }

  def solveSpectralAlignmentProblem(peptide: String,
                                    spectrum: Vector[Int],
                                    massTable: Map[Char, Int],
                                    k: Int): String = {
    val diff: Vector[Int] = peptide.map(massTable).toVector
    val nrRows: Int = diff.length + 1
    val nrCols: Int = spectrum.length + 1
    val nrLayers: Int = k + 1

    val score = new Matrix(nrRows, nrCols, nrLayers, Double.NegativeInfinity)
    val backtrack: MutableMap[(Int, Int, Int), (Int, Int, Int)] = MutableMap()
    score(0, 0, 0) = 0.0
    for {
      ix <- 1 until nrRows
      jy <- 1 until nrCols
      if jy - diff(ix - 1) >= 0
    } {
      score(ix, jy, 0) = spectrum(jy - 1) + score(ix - 1, jy - diff(ix - 1), 0)
      backtrack((ix, jy, 0)) = (ix - 1, jy - diff(ix - 1), 0)
    }

    for {
      ix <- 1 until nrRows
      jy <- 1 until nrCols
      t <- 1 until nrLayers
    } {
      val predecessors: List[(Int, Int, Int)] = getPredecessorsOfANode(ix, jy, t, diff)
      val predecessorScore: List[Double] = predecessors.map{ case (i, j, l) => score(i, j, l) }
      val (maxIndex, maxScore): ((Int, Int, Int), Double) =
        predecessors.lazyZip(predecessorScore).maxBy{ case (_, score) => score }
      score(ix, jy, t) = spectrum(jy - 1) + maxScore
      backtrack((ix, jy, t)) = maxIndex
    }

    val maximalIndex: (Int, Int, Int) = (nrRows - 1, nrCols - 1, score.argmax())
    calcHighestScoringPeptideWithModifications(backtrack, maximalIndex, peptide, diff)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val peptide: String = reader.next()
    val spectrum: Vector[Int] = convertToIntVector(reader.next())
    val k: Int = reader.next().toInt
    val result: String = solveSpectralAlignmentProblem(peptide, spectrum, IntegerMassTable, k)
    println(result)
  }
}
