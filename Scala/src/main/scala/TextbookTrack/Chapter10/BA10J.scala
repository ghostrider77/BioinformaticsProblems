package TextbookTrack.Chapter10

object BA10J {
  private type Matrix = Array[Array[Double]]
  private val Digits: Int = 4

  case class Label(labels: Vector[Char]) {
    private val labelIndices: Map[Char, Int] = labels.iterator.zipWithIndex.toMap
    val size: Int = labels.length

    override def toString: String = labels.mkString("\t")

    def apply(label: Char): Int = labelIndices(label)

    def indices: Range = labels.indices

    def zipWithIndex: Iterator[(Char, Int)] = labels.iterator.zipWithIndex
  }

  case class ProbabilityMatrix(rowLabels: Label, columnLabels: Label, probabilities: Matrix) {
    def apply(row: Char, col: Char): Double = probabilities(rowLabels(row))(columnLabels(col))
  }

  case class HMM(alphabet: Label, states: Label, transition: ProbabilityMatrix, emission: ProbabilityMatrix)

  private def round(number: Double, digits: Int): Double =
    math.rint(math.pow(10, digits) * number) / math.pow(10, digits)

  private def collectLabels(line: String): Vector[Char] =
    line.toVector.filter(_.isLetter)

  private def readMatrix(reader: Iterator[String], k: Int): Matrix = {
    val _: String = reader.next()
    reader
      .take(k)
      .map(_.split("\\s+").drop(1).map(_.toDouble))
      .toArray
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

  private def runForwardAlgorithm(hmm: HMM, emittedString: String): Matrix = {
    val HMM(_, states, transition, emission) = hmm
    val nrCols: Int = emittedString.length
    val forwardMatrix: Matrix = Array.fill(states.size, nrCols)(0.0)
    val char: Char = emittedString(0)
    for {
      (state, ix) <- states.zipWithIndex
    } forwardMatrix(ix)(0) = emission(state, char) / states.size

    for {
      (char, jy) <- emittedString.drop(1).zipWithIndex
      (state, ix) <- states.zipWithIndex
    } {
      val emissionProb: Double = emission(state, char)
      val scores: Iterator[Double] =
        states
          .zipWithIndex
          .map{ case (previousState, k) => forwardMatrix(k)(jy) * transition(previousState, state) * emissionProb }
      forwardMatrix(ix)(jy + 1) = scores.sum
    }

    forwardMatrix
  }

  private def runBackwardAlgorithm(hmm: HMM, emittedString: String): Matrix = {
    val HMM(_, states, transition, emission) = hmm
    val nrCols: Int = emittedString.length
    val backwardMatrix: Matrix = Array.fill(states.size, nrCols)(0.0)
    for {
      ix <- states.indices
    } backwardMatrix(ix)(nrCols - 1) = 1.0

    for {
      (char, jy) <- emittedString.drop(1).reverseIterator.zipWithIndex
      (state, ix) <- states.zipWithIndex
    } {
      val scores: Iterator[Double] =
        states
          .zipWithIndex
          .map{ case (nextState, k) =>
            backwardMatrix(k)(nrCols - jy - 1) * transition(state, nextState) * emission(nextState, char)
          }
      backwardMatrix(ix)(nrCols - jy - 2) = scores.sum
    }

    backwardMatrix
  }

  def solveSoftDecodingProblem(hmm: HMM, emittedString: String): Matrix = {
    val forwardMatrix: Matrix = runForwardAlgorithm(hmm, emittedString)
    val backwardMatrix: Matrix = runBackwardAlgorithm(hmm, emittedString)
    val forwardSink: Double = forwardMatrix.map(_.last).sum
    val sinkLogprobability: Double = math.log(forwardSink)
    val conditionalProbabilities: Matrix = Array.fill(emittedString.length, hmm.states.size)(0.0)
    for {
      ix <- emittedString.indices
      jy <- hmm.states.indices
    } {
      val logP: Double = math.log(forwardMatrix(jy)(ix)) + math.log(backwardMatrix(jy)(ix)) - sinkLogprobability
      conditionalProbabilities(ix)(jy) = math.exp(logP)
    }

    conditionalProbabilities
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (emittedString, hmm): (String, HMM) = readData(reader)
    val result: Matrix = solveSoftDecodingProblem(hmm, emittedString)
    println(hmm.states)
    result.foreach(line => println(line.map(round(_, Digits)).mkString("\t")))
  }
}
