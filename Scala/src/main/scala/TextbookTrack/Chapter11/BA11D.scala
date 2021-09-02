package TextbookTrack.Chapter11

object BA11D {
  import TextbookTrack.Utils.IntegerMassTable

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def reverseMassTableMapping(massTable: Map[Char, Int]): Map[Int, Char] = massTable.map(_.swap)

  private def calcPrefixMasses(peptideVector: List[Int]): List[Int] =
    0 :: peptideVector.zipWithIndex.withFilter{ case (item, _) => item == 1 }.map{ case (_, ix) => ix + 1 }

  def restorePeptideFromPeptideVector(peptideVector: List[Int], massTable: Map[Char, Int]): Option[String] = {
    val prefixMasses: List[Int] = calcPrefixMasses(peptideVector)
    val inverseMapping: Map[Int, Char] = reverseMassTableMapping(massTable)
    val peptide: List[Option[Char]] =
      (for { List(mass1, mass2) <- prefixMasses.sliding(2) } yield inverseMapping.get(mass2 - mass1)).toList
    if (peptide.exists(_.isEmpty)) None else Some(peptide.flatten.mkString)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val peptideVector: List[Int] = convertToIntList(reader.next())
    val result: Option[String] = restorePeptideFromPeptideVector(peptideVector, IntegerMassTable)
    result match {
      case Some(peptide) => println(peptide)
      case None => println("The given vector does not encode a peptide.")
    }
  }
}
