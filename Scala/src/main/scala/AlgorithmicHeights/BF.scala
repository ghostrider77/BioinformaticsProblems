package AlgorithmicHeights

object BF {
  final case class Edge(from: Int, to: Int, weight: Int)

  class DirectedGraph(val nrNodes: Int, edgeList: List[Edge]) {
    import DirectedGraph.buildAdjacencyList

    private val adjacencyList: Map[Int, List[Edge]] = buildAdjacencyList(edgeList)

    def foreach(f: ((Int, List[Edge])) => Unit): Unit = adjacencyList.foreach(f)
  }

  object DirectedGraph {
    private def buildAdjacencyList(edgeList: List[Edge]): Map[Int, List[Edge]] =
      edgeList.groupMap{ case Edge(a, _, _) => a }(identity)
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(n, e) => (n, e)
    case _ => throw new Exception("Malformed input.")
  }

  private def readEdges(reader: Iterator[String], nrEdges: Int): List[Edge] =
    reader
      .take(nrEdges)
      .map(convertToIntList)
      .collect{ case List(a, b, w) => Edge(a, b, w) }
      .toList

  def updateDistances(graph: DirectedGraph, distances: Array[Double]): Unit = {
    graph.foreach{
      case (node, neighbours) =>
        val distNode: Double = distances(node - 1)
        if (distNode.isFinite) {
          neighbours.foreach{
            case Edge(_, neighbour, weight) =>
              val distanceThroughNode: Double = distNode + weight
              if (distances(neighbour - 1) > distanceThroughNode) distances(neighbour - 1) = distanceThroughNode
          }
        }
    }
  }

  def runBellmanFordAlgorithm(graph: DirectedGraph, node: Int): List[Double] = {
    val distances: Array[Double] = Array.fill(graph.nrNodes)(Double.PositiveInfinity)
    distances(node - 1) = 0.0
    (0 until graph.nrNodes).foreach(_ => updateDistances(graph, distances))
    distances.toList
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (nrNodes, nrEdges): (Int, Int) = readParameters(reader.next())
    val edges: List[Edge] = readEdges(reader, nrEdges)
    val graph = new DirectedGraph(nrNodes, edges)
    val result: List[Double] = runBellmanFordAlgorithm(graph, node = 1)
    println(result.map(x => if (x.isPosInfinity) "x" else x.toInt.toString).mkString(" "))
  }
}
