package TextbookTrack.Chapter10

object BA10C {
  import scala.annotation.tailrec

  case class Label(labels: Vector[Char]) {
    private val labelIndices: Map[Char, Int] = labels.iterator.zipWithIndex.toMap
    val size: Int = labels.length

    def apply(label: Char): Int = labelIndices(label)

    def zipWithIndex: Iterator[(Char, Int)] = labels.iterator.zipWithIndex
  }

  case class ProbabilityMatrix(rowLabels: Label, columnLabels: Label, probabilities: Vector[Vector[Double]]) {
    val nrRows: Int = rowLabels.size
    val nrCols: Int = columnLabels.size

    def apply(row: Char, col: Char): Double = probabilities(rowLabels(row))(columnLabels(col))
  }

  case class HMM(alphabet: Label, states: Label, transition: ProbabilityMatrix, emission: ProbabilityMatrix)

  private def collectLabels(line: String): Vector[Char] =
    line.toVector.filter(_.isLetter)

  private def readMatrix(reader: Iterator[String], k: Int): Vector[Vector[Double]] = {
    val _: String = reader.next()
    reader
      .take(k)
      .map(_.split("\\s").drop(1).map(_.toDouble).toVector)
      .toVector
  }

  private def readData(reader: Iterator[String]): (String, HMM) = {
    val string: String = reader.next()
    val _: String = reader.next()
    val alphabet = Label(collectLabels(reader.next()))
    val _: String = reader.next()
    val states = Label(collectLabels(reader.next()))
    val _: String = reader.next()
    val transition = ProbabilityMatrix(states, states, readMatrix(reader, states.size))
    val _: String = reader.next()
    val emission = ProbabilityMatrix(states, alphabet, readMatrix(reader, states.size))
    val hmm = HMM(alphabet, states, transition, emission)
    (string, hmm)
  }

  private def reconstructOptimalHiddenPath(backtrack: Array[Array[Int]],
                                           nrCols: Int,
                                           maximalScoreIndex: Int,
                                           states: Label): String = {
    @tailrec
    def loop(hiddenPath: List[Char], jy: Int, index: Int): String = {
      if (jy < 0) hiddenPath.mkString
      else {
        val maximalIndex: Int = backtrack(index)(jy)
        loop(states.labels(maximalIndex) :: hiddenPath, jy - 1, maximalIndex)
      }
    }
    loop(List(states.labels(maximalScoreIndex)), nrCols - 1, maximalScoreIndex)
  }

  def calcMostLikelyHiddenPath(hmm: HMM, emittedString: String): String = {
    val HMM(_, states, transition, emission) = hmm
    val nrCols: Int = emittedString.length
    val logscoreMatrix: Array[Array[Double]] = Array.fill(states.size, nrCols)(0.0)
    val backtrack: Array[Array[Int]] = Array.fill(states.size, nrCols - 1)(0)
    val char: Char = emittedString(0)
    for {
      (state, ix) <- states.zipWithIndex
    } logscoreMatrix(ix)(0) = math.log(1.0 / states.size) + math.log(emission(state, char))

    for {
      (char, jy) <- emittedString.drop(1).zipWithIndex
      (state, ix) <- states.zipWithIndex
    } {
      val emissionLogprob: Double = math.log(emission(state, char))
      val logScores: Iterator[Double] =
        states
          .zipWithIndex
          .map{
            case (previousState, k) =>
              logscoreMatrix(k)(jy) + math.log(transition(previousState, state)) + emissionLogprob
          }
      val (maximumSumWeight, index): (Double, Int) = logScores.zipWithIndex.maxBy{ case (score, _) => score }
      logscoreMatrix(ix)(jy + 1) = maximumSumWeight
      backtrack(ix)(jy) = index
    }

    val (_, maximalScoreIndex): (Double, Int) =
      logscoreMatrix.map(_.last).zipWithIndex.maxBy{ case (score, _) => score }
    reconstructOptimalHiddenPath(backtrack, nrCols - 1, maximalScoreIndex, states)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (emittedString, hmm): (String, HMM) = readData(reader)
    val result: String = calcMostLikelyHiddenPath(hmm, emittedString)
    println(result)
  }
}
