package TextbookTrack.Chapter10

object BA10I {
  import scala.annotation.tailrec

  private type Matrix = Array[Array[Double]]
  private val Digits: Int = 3
  private val Epsilon: Double = 1e-10

  case class Label(labels: Vector[Char]) {
    private val labelIndices: Map[Char, Int] = labels.iterator.zipWithIndex.toMap
    val size: Int = labels.length

    def apply(label: Char): Int = labelIndices(label)

    def foreach(f: Char => Unit): Unit = labels.foreach(f)

    def map[T](f: Char => T): Vector[T] = labels.map(f)

    def zipWithIndex: Iterator[(Char, Int)] = labels.iterator.zipWithIndex
  }

  class ProbabilityMatrix(rowLabels: Label, columnLabels: Label, probs: Option[Matrix] = None) {
    val nrRows: Int = rowLabels.size
    val nrCols: Int = columnLabels.size
    val probabilities: Matrix = initializeMatrix(probs)

    override def toString: String = {
      val header: String = columnLabels.map(_.toString).mkString("\t", "\t", "")
      val lines: Vector[String] =
        rowLabels.map { label =>
          val probs: Array[Double] = probabilities(rowLabels(label))
          s"$label\t${probs.map(round(_, Digits)).mkString("\t")}"
        }
      s"$header\n${lines.mkString("\n")}"
    }

    private def initializeMatrix(probs: Option[Matrix]): Matrix = probs match {
      case Some(matrix) => matrix
      case None => Array.fill(nrRows, nrCols)(Epsilon)
    }

    def apply(row: Char, col: Char): Double = probabilities(rowLabels(row))(columnLabels(col))

    def update(row: Char, col: Char, value: Double): Unit = probabilities(rowLabels(row))(columnLabels(col)) = value

    def rowSum(row: Char): Double = probabilities(rowLabels(row)).sum
  }

  case class HMM(alphabet: Label, states: Label, transition: ProbabilityMatrix, emission: ProbabilityMatrix)

  private def round(number: Double, digits: Int): Double =
    math.rint(math.pow(10, digits) * number) / math.pow(10, digits)

  private def collectLabels(line: String): Vector[Char] =
    line.toVector.filter(_.isLetter)

  private def readMatrix(reader: Iterator[String], k: Int): Array[Array[Double]] = {
    val _: String = reader.next()
    reader
      .take(k)
      .map(_.split("\\s+").drop(1).map(_.toDouble))
      .toArray
  }

  private def readData(reader: Iterator[String]): (String, HMM, Int) = {
    val n: Int = reader.next().toInt
    val _: String = reader.next()
    val string: String = reader.next()
    val _: String = reader.next()
    val alphabet = Label(collectLabels(reader.next()))
    val _: String = reader.next()
    val states = Label(collectLabels(reader.next()))
    val _: String = reader.next()
    val transition = new ProbabilityMatrix(states, states, Some(readMatrix(reader, states.size)))
    val _: String = reader.next()
    val emission = new ProbabilityMatrix(states, alphabet, Some(readMatrix(reader, states.size)))
    val hmm = HMM(alphabet, states, transition, emission)
    (string, hmm, n)
  }

  private def estimateTransitionProbabilities(hiddenPath: String, states: Label): ProbabilityMatrix = {
    val transition = new ProbabilityMatrix(states, states)
    hiddenPath
      .sliding(2)
      .map(_.toList)
      .collect { case List(stateFrom, stateTo) => (stateFrom, stateTo) }
      .foreach { case (stateFrom, stateTo) => transition(stateFrom, stateTo) += 1 }

    for {
      s1 <- states
      rowSum: Double = transition.rowSum(s1)
      s2 <- states
    } transition(s1, s2) /= rowSum

    transition
  }

  private def estimateEmissionProbabilities(emittedString: String,
                                            hiddenPath: String,
                                            alphabet: Label,
                                            states: Label): ProbabilityMatrix = {
    val emission = new ProbabilityMatrix(states, alphabet)
    hiddenPath.lazyZip(emittedString).foreach { case (state, char) => emission(state, char) += 1 }

    for {
      state <- states
      rowSum: Double = emission.rowSum(state)
      letter <- alphabet
    } emission(state, letter) /= rowSum

    emission
  }

  private def estimateHmmParameters(emittedString: String, hiddenPath: String, alphabet: Label, states: Label): HMM = {
    val transition: ProbabilityMatrix = estimateTransitionProbabilities(hiddenPath, states)
    val emission: ProbabilityMatrix = estimateEmissionProbabilities(emittedString, hiddenPath, alphabet, states)
    HMM(alphabet, states, transition, emission)
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

  private def calcMostLikelyHiddenPath(hmm: HMM, emittedString: String): String = {
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

  def runViterbiLearning(initialHmm: HMM, emittedString: String, n: Int): HMM =
    (0 until n).foldLeft(initialHmm){
      case (hmm, _) =>
        val hiddenPath: String = calcMostLikelyHiddenPath(hmm, emittedString)
        estimateHmmParameters(emittedString, hiddenPath, hmm.alphabet, hmm.states)
    }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (emittedString, hmm, n): (String, HMM, Int) = readData(reader)
    val result: HMM = runViterbiLearning(hmm, emittedString, n)
    println(result.transition)
    println("--------")
    println(result.emission)
  }
}
