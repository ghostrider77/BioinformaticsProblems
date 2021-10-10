package TextbookTrack.Chapter06

object BA6G {
  sealed trait Sign
  final case object Plus extends Sign {
    override def toString: String = "+"
  }
  final case object Minus extends Sign {
    override def toString: String = "-"
  }

  object Sign {
    def apply(char: Char): Sign = char match {
      case '+' => Plus
      case '-' => Minus
      case _ => throw new Exception("Unknown sign.")
    }
  }

  final case class SignedNumber(sign: Sign, number: Int) {
    override def toString: String = s"$sign$number"
  }

  final case class Chromosome(syntenyBlocks: Vector[SignedNumber]) {
    override def toString: String = syntenyBlocks.mkString("(", " ", ")")
  }

  final case class Cycle(nodes: Vector[Int]) {
    override def toString: String = nodes.mkString("(", " ", ")")

    def grouped(size: Int): Iterator[Vector[Int]] = nodes.grouped(size)
  }

  def readCycle(line: String): Cycle = {
    val nodes: Vector[Int] =
      line
        .dropRight(1)
        .drop(1)
        .split(" ")
        .map(_.toInt)
        .toVector
    Cycle(nodes)
  }

  def cycleToChromosome(cycle: Cycle): Chromosome = {
    val syntenyBlocks: Vector[SignedNumber] =
      cycle
        .grouped(2)
        .collect{
          case Vector(node1, node2) =>
            if (node1 < node2) SignedNumber(Plus, node2 / 2)
            else SignedNumber(Minus, node1 / 2)
        }
        .toVector
    Chromosome(syntenyBlocks)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val cycle: Cycle = readCycle(reader.next())
    val result: Chromosome = cycleToChromosome(cycle)
    println(result)
  }
}
