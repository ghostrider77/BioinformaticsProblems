package TextbookTrack.Chapter10

object BA10H {
  private type Matrix = Array[Array[Double]]
  private val Digits: Int = 3

  case class Label(labels: Vector[Char]) {
    private val labelIndices: Map[Char, Int] = labels.iterator.zipWithIndex.toMap
    val size: Int = labels.length

    def apply(label: Char): Int = labelIndices(label)

    def foreach(f: Char => Unit): Unit = labels.foreach(f)

    def map[T](f: Char => T): Vector[T] = labels.map(f)
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
      case None => Array.fill(nrRows, nrCols)(0.0)
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

  private def readData(reader: Iterator[String]): (String, String, Label, Label) = {
    val string: String = reader.next()
    val _: String = reader.next()
    val alphabet = Label(collectLabels(reader.next()))
    val _: String = reader.next()
    val hiddenPath: String = reader.next()
    val _: String = reader.next()
    val states = Label(collectLabels(reader.next()))
    (string, hiddenPath, alphabet, states)
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
    } {
      transition(s1, s2) = if (rowSum == 0) 1.0 / states.size else transition(s1, s2) / rowSum
    }

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
    } {
      emission(state, letter) = if (rowSum == 0) 1.0 / alphabet.size else emission(state, letter) / rowSum
    }

    emission
  }

  def estimateHmmParameters(emittedString: String, hiddenPath: String, alphabet: Label, states: Label): HMM = {
    val transition: ProbabilityMatrix = estimateTransitionProbabilities(hiddenPath, states)
    val emission: ProbabilityMatrix = estimateEmissionProbabilities(emittedString, hiddenPath, alphabet, states)
    HMM(alphabet, states, transition, emission)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (emittedString, hiddenPath, alphabet, states): (String, String, Label, Label) = readData(reader)
    val result: HMM = estimateHmmParameters(emittedString, hiddenPath, alphabet, states)
    println(result.transition)
    println("--------")
    println(result.emission)
  }
}
