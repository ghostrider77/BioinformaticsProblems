package Stronghold

object RNA {
  def transcribeDna(dna: String): String =
    dna.map(nucleotide => if (nucleotide == 'T') 'U' else nucleotide)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val dna: String = reader.next()
    val result: String = transcribeDna(dna)
    println(result)
  }
}
