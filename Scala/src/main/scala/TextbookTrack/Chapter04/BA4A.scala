package TextbookTrack.Chapter04

object BA4A {
  import TextbookTrack.Utils.GeneticCode

  def translateRNA(rna: String): String =
    rna.grouped(3).map(GeneticCode.get).takeWhile(_.nonEmpty).flatten.mkString

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val rna: String = reader.next()
    val result: String = translateRNA(rna)
    println(result)
  }
}
