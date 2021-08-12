package TextbookTrack.Chapter10

object BA10D {
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

  def calcProbabilityOfEmittedString(hmm: HMM, emittedString: String): Double = {
    val HMM(_, states, transition, emission) = hmm
    val nrCols: Int = emittedString.length
    val forwardMatrix: Array[Array[Double]] = Array.fill(states.size, nrCols)(0.0)
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

    forwardMatrix.map(_.last).sum
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (emittedString, hmm): (String, HMM) = readData(reader)
    val result: Double = calcProbabilityOfEmittedString(hmm, emittedString)
    println(result)
  }
}
