package TextbookTrack.Chapter04

object BA4D {
  import TextbookTrack.Utils.IntegerMassTable
  import scala.collection.mutable.{Map => MutableMap}

  def calcNumberOfPeptidesWithGivenMass(totalMass: Int): Long = {
    val aminoAcidMasses: Set[Int] = IntegerMassTable.values.toSet
    val cache: MutableMap[Int, Long] = MutableMap()

    def solve(currentMass: Int): Long = {
      def nrPeptides: Long =
        if (currentMass < 0) 0L
        else if (currentMass == 0) 1L
        else aminoAcidMasses.foldLeft(0L)((acc, mass) => acc + solve(currentMass - mass))
      cache.getOrElseUpdate(currentMass, nrPeptides)
    }

    if (totalMass <= 0) 0L else solve(totalMass)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val totalMass: Int = reader.next().toInt
    val result: Long = calcNumberOfPeptidesWithGivenMass(totalMass)
    println(result)
  }
}
