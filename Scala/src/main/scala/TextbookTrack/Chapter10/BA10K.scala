package TextbookTrack.Chapter10

object BA10K {
  private type Matrix = Array[Array[Double]]
  private val Digits: Int = 3
  private val Epsilon: Double = 1e-10

  case class Label(labels: Vector[Char]) {
    private val labelIndices: Map[Char, Int] = labels.iterator.zipWithIndex.toMap
    val size: Int = labels.length

    def apply(label: Char): Int = labelIndices(label)

    def foreach(f: Char => Unit): Unit = labels.foreach(f)

    def map[T](f: Char => T): Vector[T] = labels.map(f)

    def indices: Range = labels.indices

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

  private def estimateTransitionProbabilities(states: Label,
                                              emittedString: String,
                                              responsibilities: Map[(Char, Char, Int), Double]): ProbabilityMatrix = {
    val transition = new ProbabilityMatrix(states, states)
    val n: Int = emittedString.length
    for {
      stateL <- states
      stateK <- states
    } transition(stateL, stateK) = (0 until n - 1).map(ix => responsibilities((stateL, stateK, ix))).sum

    for {
      s1 <- states
      rowSum: Double = transition.rowSum(s1)
      s2 <- states
    } transition(s1, s2) /= rowSum

    transition
  }

  private def estimateEmissionProbabilities(states: Label,
                                            alphabet: Label,
                                            emittedString: String,
                                            responsibilities: Map[(Char, Int), Double]): ProbabilityMatrix = {
    val emission = new ProbabilityMatrix(states, alphabet)
    for {
      state <- states
      letter <- alphabet
    } {
      val emissionProbabilities: Iterator[Double] =
        emittedString
          .iterator
          .zipWithIndex
          .collect{ case (char, ix) if char == letter => responsibilities((state, ix)) }
      emission(state, letter) = emissionProbabilities.sum
    }

    for {
      state <- states
      rowSum: Double = emission.rowSum(state)
      letter <- alphabet
    } emission(state, letter) /= rowSum

    emission
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

  private def calcNodeResponsibilities(forwardMatrix: Matrix,
                                       backwardMatrix: Matrix,
                                       states: Label,
                                       emittedString: String): Map[(Char, Int), Double] = {
    val forwardSink: Double = forwardMatrix.map(_.last).sum
    val sinkLogprobability: Double = math.log(forwardSink)
    (for {
      jy <- emittedString.indices.iterator
      (state, ix) <- states.zipWithIndex
      logP: Double = math.log(forwardMatrix(ix)(jy)) + math.log(backwardMatrix(ix)(jy)) - sinkLogprobability
    } yield (state, jy) -> math.exp(logP)).toMap
  }

  private def calcEdgeResponsibilities(forwardMatrix: Matrix,
                                       backwardMatrix: Matrix,
                                       hmm: HMM,
                                       emittedString: String): Map[(Char, Char, Int), Double] = {
    val HMM(_, states, transition, emission) = hmm
    val forwardSink: Double = forwardMatrix.map(_.last).sum
    val sinkLogprobability: Double = math.log(forwardSink)
    (for {
      (nextChar, ix) <- emittedString.iterator.drop(1).zipWithIndex
      (stateK, kx) <- states.zipWithIndex
      (stateL, lx) <- states.zipWithIndex
      logEdgeWeight: Double = math.log(transition(stateL, stateK)) + math.log(emission(stateK, nextChar))
      logP: Double = math.log(forwardMatrix(lx)(ix)) + math.log(backwardMatrix(kx)(ix + 1)) +
        logEdgeWeight - sinkLogprobability
    } yield (stateL, stateK, ix) -> math.exp(logP)).toMap
  }

  private def expectationStep(hmm: HMM,
                              emittedString: String): (Map[(Char, Int), Double], Map[(Char, Char, Int), Double]) = {
    val forwardMatrix: Matrix = runForwardAlgorithm(hmm, emittedString)
    val backwardMatrix: Matrix = runBackwardAlgorithm(hmm, emittedString)
    val nodeResponsibilities: Map[(Char, Int), Double] =
      calcNodeResponsibilities(forwardMatrix, backwardMatrix, hmm.states, emittedString)
    val edgeResponsibilities: Map[(Char, Char, Int), Double] =
      calcEdgeResponsibilities(forwardMatrix, backwardMatrix, hmm, emittedString)
    (nodeResponsibilities, edgeResponsibilities)
  }

  private def maximizationStep(hmm: HMM,
                               emittedString: String,
                               nodeResponsibilities: Map[(Char, Int), Double],
                               edgeResponsibilities: Map[(Char, Char, Int), Double]): HMM = {
    val HMM(alphabet, states, _, _) = hmm
    val transition: ProbabilityMatrix = estimateTransitionProbabilities(states, emittedString, edgeResponsibilities)
    val emission: ProbabilityMatrix =
      estimateEmissionProbabilities(states, alphabet, emittedString, nodeResponsibilities)
    HMM(alphabet, states, transition, emission)
  }

  def runBaumWelchLearning(initialHmm: HMM, emittedString: String, n: Int): HMM =
    (0 until n).foldLeft(initialHmm){
      case (hmm, _) =>
        val (nodeResponsibilities, edgeResponsibilities): (Map[(Char, Int), Double], Map[(Char, Char, Int), Double]) =
          expectationStep(hmm, emittedString)
        maximizationStep(hmm, emittedString, nodeResponsibilities, edgeResponsibilities)
    }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (emittedString, hmm, n): (String, HMM, Int) = readData(reader)
    val result: HMM = runBaumWelchLearning(hmm, emittedString, n)
    println(result.transition)
    println("--------")
    println(result.emission)
  }
}
