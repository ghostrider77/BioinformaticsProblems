package TextbookTrack.Chapter03

object BA3D {
  class DeBruijnGraph(text: String, k: Int) {
    import DeBruijnGraph.buildDeBruijnGraph

    def edges: Iterator[String] =
      for {
        (kMer, neighbours) <- adjacencyList.iterator
        neighbourString: String = neighbours.mkString(",")
      } yield s"$kMer -> $neighbourString"

    private val adjacencyList: Map[String, List[String]] = buildDeBruijnGraph(text, k)
  }

  object DeBruijnGraph {
    private def buildDeBruijnGraph(text: String, k: Int): Map[String, List[String]] =
      text
        .sliding(k)
        .map(kMer => (kMer.dropRight(1), kMer.drop(1)))
        .toList
        .groupBy{ case (prefix, _) => prefix }
        .view
        .mapValues(_.map{ case (_, suffix) => suffix })
        .toMap
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val k: Int = reader.next().toInt
    val text: String = reader.next()
    val graph = new DeBruijnGraph(text, k)
    graph.edges.foreach(println)
  }
}
