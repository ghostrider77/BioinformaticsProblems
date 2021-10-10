package TextbookTrack.Chapter06

object BA6H {
  import scala.util.matching.Regex

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

    def length: Int = syntenyBlocks.length

    def flatMap[T](f: SignedNumber => Vector[T]): Vector[T] = syntenyBlocks.flatMap(f)
  }

  final case class Cycle(nodes: Vector[Int]) {
    override def toString: String = nodes.mkString("(", " ", ")")

    private val length: Int = nodes.length

    def apply(k: Int): Int = {
      if (k == length) nodes(0)
      else nodes(k)
    }
  }

  final case class Genome(chromosomes: List[Chromosome]) {
    override def toString: String = chromosomes.map(_.toString).mkString("\n")

    def flatMap[T](f: Chromosome => List[T]): List[T] = chromosomes.flatMap(f)
  }

  final case class Edge(a: Int, b: Int) {
    override def toString: String = s"($a, $b)"
  }

  def readGenome(line: String): Genome = {
    val pattern: Regex = "\\((.*?)\\)".r
    val chromosomes: List[Chromosome] =
      pattern
        .findAllIn(line)
        .map{ part =>
          val syntenyBlocks: Vector[SignedNumber] =
            part
              .dropRight(1)
              .drop(1)
              .split(" ")
              .map{ item => SignedNumber(Sign(item(0)), item.drop(1).toInt) }
              .toVector
          Chromosome(syntenyBlocks)
        }
        .toList
    Genome(chromosomes)
  }

  private def chromosomeToCycle(chromosome: Chromosome): Cycle = {
    val nodes: Vector[Int] = chromosome.flatMap{
      case SignedNumber(sign, number) =>
        if (sign == Plus) Vector(2*number - 1, 2*number)
        else Vector(2*number, 2*number - 1)
    }
    Cycle(nodes)
  }

  def calcColoredEdges(genome: Genome): List[Edge] = {
    genome.flatMap{ chromosome =>
      val cycle: Cycle = chromosomeToCycle(chromosome)
      val n: Int = chromosome.length
      (1 to n).map(ix => Edge(cycle(2*ix - 1), cycle(2*ix))).toList
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val genome: Genome = readGenome(reader.next())
    val result: List[Edge] = calcColoredEdges(genome)
    println(result.map(_.toString).mkString(", "))
  }
}
