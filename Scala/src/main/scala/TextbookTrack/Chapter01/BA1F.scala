package TextbookTrack.Chapter01

object BA1F {
  def getSkewnessArgmins(genome: String): List[Int] = {
    val (_, _, argmins): (Int, Int, List[Int]) =
      genome.view.zipWithIndex.foldLeft((0, 0, List(0))){
        case ((skew, minValue, minIndices), (nucleotide, ix)) =>
          val updatedSkew: Int = if (nucleotide == 'C') skew - 1 else if (nucleotide == 'G') skew + 1 else skew
          if (updatedSkew == minValue) (updatedSkew, minValue, (ix + 1) :: minIndices)
          else if (updatedSkew < minValue) (updatedSkew, updatedSkew, List(ix + 1))
          else (updatedSkew, minValue, minIndices)
      }
    argmins.reverse
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val genome: String = reader.next()
    val result: List[Int] = getSkewnessArgmins(genome)
    println(result.mkString(" "))
  }
}
