package Stronghold

object DNA {
  private val Nucleotides: Vector[Char] = Vector('A', 'C', 'G', 'T')

  def countNucleotides(dna: String): Vector[Int] = {
    val counts: Map[Char, Int] = dna.groupMapReduce(identity)(_ => 1)(_ + _)
    Nucleotides.map(counts.getOrElse(_, 0))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val dna: String = reader.next()
    val result: Vector[Int] = countNucleotides(dna)
    println(result.mkString(" "))
  }
}
