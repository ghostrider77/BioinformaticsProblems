package TextbookTrack.Chapter11

object BA11C {
  import TextbookTrack.Utils.IntegerMassTable

  def calcPeptideVector(peptide: String, massTable: Map[Char, Int]): List[Int] = {
    val prefixMasses: Vector[Int] = peptide.scanLeft(0)((acc, aminoAcid) => acc + massTable(aminoAcid)).toVector
    val totalMass: Int = prefixMasses.last
    val peptideVector: Array[Int] = Array.fill(totalMass)(0)
    prefixMasses.drop(1).foreach(mass => peptideVector(mass - 1) = 1)
    peptideVector.toList
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val peptide: String = reader.next()
    val result: List[Int] = calcPeptideVector(peptide, IntegerMassTable)
    println(result.mkString(" "))
  }
}
