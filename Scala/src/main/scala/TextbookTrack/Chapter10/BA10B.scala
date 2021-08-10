package TextbookTrack.Chapter10

object BA10B {
  final case class Alphabet(alphabet: Seq[Char]) {
    private val charIndices: Map[Char, Int] = createIndexedMap(alphabet)

    def apply(state: Char): Int = charIndices(state)
  }

  final case class States(states: Seq[Char]) {
    private val stateIndices: Map[Char, Int] = createIndexedMap(states)

    def apply(state: Char): Int = stateIndices(state)
  }

  final case class Emission(states: States, alphabet: Alphabet, probabilities: Vector[Vector[Double]]) {
    def apply(state: Char, char: Char): Double = probabilities(states(state))(alphabet(char))
  }

  private def createIndexedMap[T](sequence: Seq[T]): Map[T, Int] =
    sequence.iterator.zipWithIndex.toMap

  private def readData(reader: Iterator[String]): (String, String, Emission) = {
    val emittedString: String = reader.next()
    val _: String = reader.next()
    val alphabet = Alphabet(reader.next().toList.filter(_.isLetter))
    val _: String = reader.next()
    val hiddenPath: String = reader.next()
    val _: String = reader.next()
    val states = States(reader.next().toList.filter(_.isLetter))
    val _: String = reader.next()
    val _: String = reader.next()
    val probabilities: Vector[Vector[Double]] = reader.map(_.split("\\s").drop(1).map(_.toDouble).toVector).toVector
    (emittedString, hiddenPath, Emission(states, alphabet, probabilities))
  }

  def calcProbabilityOfEmittedStringGivenPath(emittedString: String, hiddenPath: String, emission: Emission): Double = {
    val logP: Double =
      hiddenPath
        .lazyZip(emittedString)
        .foldLeft(0.0){
          case (acc, (state, char)) => acc + math.log(emission(state, char))
        }
    math.exp(logP)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (emittedString, hiddenPath, emission): (String, String, Emission) = readData(reader)
    val result: Double = calcProbabilityOfEmittedStringGivenPath(emittedString, hiddenPath, emission)
    println(result)
  }
}
