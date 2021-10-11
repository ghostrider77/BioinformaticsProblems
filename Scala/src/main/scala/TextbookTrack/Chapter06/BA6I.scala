package TextbookTrack.Chapter06

object BA6I {
  import scala.annotation.tailrec
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
  }

  final case class Cycle(nodes: Vector[Int]) {
    override def toString: String = nodes.mkString("(", " ", ")")

    def grouped(size: Int): Iterator[Vector[Int]] = nodes.grouped(size)
  }

  final case class Genome(chromosomes: List[Chromosome]) {
    override def toString: String = chromosomes.map(_.toString).mkString
  }

  final case class Edge(a: Int, b: Int) {
    override def toString: String = s"($a, $b)"
  }

  private def readGenomeGraphEdges(line: String): List[Edge] = {
    val pattern: Regex = "\\((.*?)\\)".r
    pattern
      .findAllIn(line)
      .map(_.dropRight(1).drop(1).split(", ").map(_.toInt).toList)
      .collect{ case List(a, b) => Edge(a, b) }
      .toList
  }

  private def cycleToChromosome(cycle: Cycle): Chromosome = {
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

  private def identifyCycles(coloredEdges: List[Edge]): List[Cycle] = {
    @tailrec
    def loop(cycles: List[Cycle],
             currentNodes: List[Int],
             startNode: Option[Int],
             edges: List[Edge]): List[Cycle] = edges match {
      case Nil => cycles.reverse
      case Edge(a, b) :: rest =>
        val start: Int = startNode match {
          case None => a
          case Some(node) => node
        }
        val potentialLast: Int = if (b % 2 == 0) b - 1 else b + 1
        if (potentialLast == start) {
          val nodes: List[Int] = b :: (a :: currentNodes).reverse
          val cycle: Cycle = Cycle(nodes.toVector)
          loop(cycle :: cycles, Nil, None, rest)
        } else loop(cycles, b :: a :: currentNodes, Some(start), rest)
    }

    loop(Nil, Nil, None, coloredEdges)
  }

  def graphToGenome(coloredEdges: List[Edge]): Genome = {
    val cycles: List[Cycle] = identifyCycles(coloredEdges)
    val chromosomes: List[Chromosome] = cycles.map(cycleToChromosome)
    Genome(chromosomes)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val coloredEdges: List[Edge] = readGenomeGraphEdges(reader.next())
    val result: Genome = graphToGenome(coloredEdges)
    println(result)
  }
}
