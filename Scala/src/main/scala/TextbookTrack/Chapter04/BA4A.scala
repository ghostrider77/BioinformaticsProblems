package TextbookTrack.Chapter04

object BA4A {
  import TextbookTrack.Utils.readGeneticCode

  def translateRNA(rna: String, geneticCode: Map[String, String]): String =
    rna.grouped(3).map(geneticCode.get).takeWhile(_.nonEmpty).flatten.mkString

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val rna: String = reader.next()
    val geneticCode: Map[String, String] = readGeneticCode()
    val result: String = translateRNA(rna, geneticCode)
    println(result)
  }
}
