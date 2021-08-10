package TextbookTrack.Chapter10

object BA10A {
  final case class States(states: Seq[Char]) {
    private val stateIndices: Map[Char, Int] = states.iterator.zipWithIndex.toMap
    val nrStates: Int = states.length

    def apply(state: Char): Int = stateIndices(state)
  }

  final case class Transition(states: States, probabilities: Vector[Vector[Double]]) {
    def apply(s1: Char, s2: Char): Double = probabilities(states(s1))(states(s2))
  }

  private def readData(reader: Iterator[String]): (List[Char], Transition) = {
    val hiddenPath: List[Char] = reader.next().toList
    val _: String = reader.next()
    val states = States(reader.next().toList.filter(_.isLetter))
    val _: String = reader.next()
    val _: String = reader.next()
    val probabilities: Vector[Vector[Double]] = reader.map(_.split("\\s").drop(1).map(_.toDouble).toVector).toVector
    (hiddenPath, Transition(states, probabilities))
  }

  def calcProbabilityOfHiddenPath(hiddenPath: List[Char], transition: Transition): Double = {
    val initialTransitionProbability: Double = 1.0 / transition.states.nrStates
    val logP: Double =
      hiddenPath
        .sliding(2)
        .collect{ case List(s1, s2) => (s1, s2) }
        .foldLeft(math.log(initialTransitionProbability)){
          case (acc, (s1, s2)) => acc + math.log(transition(s1, s2))
        }
    math.exp(logP)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (hiddenPath, transition): (List[Char], Transition) = readData(reader)
    val result: Double = calcProbabilityOfHiddenPath(hiddenPath, transition)
    println(result)
  }
}
