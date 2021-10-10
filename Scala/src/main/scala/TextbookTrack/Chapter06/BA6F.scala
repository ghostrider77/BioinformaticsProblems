package TextbookTrack.Chapter06

object BA6F {
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

    def flatMap[T](f: SignedNumber => Vector[T]): Vector[T] = syntenyBlocks.flatMap(f)
  }

  final case class Cycle(nodes: Vector[Int]) {
    override def toString: String = nodes.mkString("(", " ", ")")
  }

  def readChromosome(line: String): Chromosome = {
    val syntenyBlocks: Vector[SignedNumber] =
      line
        .dropRight(1)
        .drop(1)
        .split(" ")
        .map{ item => SignedNumber(Sign(item(0)), item.drop(1).toInt) }
        .toVector
    Chromosome(syntenyBlocks)
  }

  def chromosomeToCycle(chromosome: Chromosome): Cycle = {
    val nodes: Vector[Int] = chromosome.flatMap{
      case SignedNumber(sign, number) =>
        if (sign == Plus) Vector(2*number - 1, 2*number)
        else Vector(2*number, 2*number - 1)
    }
    Cycle(nodes)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val chromosome: Chromosome = readChromosome(reader.next())
    val result: Cycle = chromosomeToCycle(chromosome)
    println(result)
  }
}
