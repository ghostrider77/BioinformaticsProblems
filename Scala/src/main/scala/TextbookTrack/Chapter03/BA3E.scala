package TextbookTrack.Chapter03

object BA3E {
  class DeBruijnGraph(kMers: List[String]) {
    import DeBruijnGraph.buildDeBruijnGraph

    def edges: Iterator[String] =
      for {
        (kMer, neighbours) <- adjacencyList.iterator
        neighbourString: String = neighbours.mkString(",")
      } yield s"$kMer -> $neighbourString"

    private val adjacencyList: Map[String, List[String]] = buildDeBruijnGraph(kMers)
  }

  object DeBruijnGraph {
    private def buildDeBruijnGraph(kMers: List[String]): Map[String, List[String]] =
      kMers
        .map(kMer => (kMer.dropRight(1), kMer.drop(1)))
        .groupBy{ case (prefix, _) => prefix }
        .view
        .mapValues(_.map{ case (_, suffix) => suffix })
        .toMap
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val graph = new DeBruijnGraph(reader.toList)
    graph.edges.foreach(println)
  }
}
