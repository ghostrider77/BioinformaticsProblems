package TextbookTrack.Chapter03

object BA3C {
  class OverlapGraph(kMers: List[String]) {
    import OverlapGraph.buildOverlapGraph

    override def toString: String = edgeList.mkString("\n")

    def edgeList: List[String] =
      (for {
        (kMer, neighbours) <- adjacencyList
        neighbour <- neighbours
      } yield s"$kMer -> $neighbour").toList

    private val adjacencyList: Map[String, List[String]] = buildOverlapGraph(kMers)
  }

  object OverlapGraph {
    private def buildOverlapGraph(kMers: List[String]): Map[String, List[String]] = {
      val prefixToPattern: Map[String, List[String]] =
        kMers
          .map(kMer => (kMer, kMer.dropRight(1)))
          .groupBy{ case (_, prefix) => prefix }
          .view
          .mapValues(_.map{ case (kMer, _) => kMer })
          .toMap

      kMers
        .groupBy(identity)
        .view
        .mapValues(_.flatMap(kMer => prefixToPattern.getOrElse(kMer.drop(1), Nil)))
        .toMap
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val kMers: List[String] = reader.toList
    val result = new OverlapGraph(kMers)
    println(result)
  }
}
