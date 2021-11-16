package AlgorithmicHeights

object BFS {
  import scala.collection.mutable.{Queue => MutableQueue}

  final case class Edge(a: Int, b: Int)

  class DirectedGraph(nrNodes: Int, edgeList: List[Edge]) {
    import DirectedGraph.buildAdjacencyList

    private val adjacencyList: Map[Int, List[Int]] = buildAdjacencyList(edgeList)

    def breadthFirstSearch(startNode: Int): List[Int] = {
      val distances: Array[Int] = Array.fill(nrNodes)(-1)
      distances(startNode - 1) = 0
      val queue: MutableQueue[Int] = MutableQueue(startNode)
      while (queue.nonEmpty) {
        val node: Int = queue.dequeue()
        val neighbours: List[Int] = adjacencyList.getOrElse(node, Nil)
        for { neighbour <- neighbours } {
          if (distances(neighbour - 1) == -1) {
            queue.enqueue(neighbour)
            distances(neighbour - 1) = distances(node - 1) + 1
          }
        }
      }
      distances.toList
    }
  }

  object DirectedGraph {
    private def buildAdjacencyList(edgeList: List[Edge]): Map[Int, List[Int]] =
      edgeList.groupMap{ case Edge(a, _) => a }{ case Edge(_, b) => b }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(n, e) => (n, e)
    case _ => throw new Exception("Malformed input.")
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (nrNodes, nrEdges): (Int, Int) = readParameters(reader.next())
    val edgeList: List[Edge] =
      reader
        .take(nrEdges)
        .map(convertToIntList)
        .collect{ case List(a, b) => Edge(a, b) }
        .toList
    val graph = new DirectedGraph(nrNodes, edgeList)
    val result: List[Int] = graph.breadthFirstSearch(startNode = 1)
    println(result.mkString(" "))
  }
}
