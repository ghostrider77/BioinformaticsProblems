package AlgorithmicHeights

object DIJ {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Set => MutableSet}

  final case class Edge(from: Int, to: Int, weight: Int)

  class DirectedGraph(val nrNodes: Int, edgeList: List[Edge]) {
    import DirectedGraph.buildAdjacencyList

    private val adjacencyList: Map[Int, List[Edge]] = buildAdjacencyList(edgeList)

    def neighbours(node: Int): List[Edge] = adjacencyList.getOrElse(node, Nil)
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

  def calcShortestDistances(graph: DirectedGraph, sourceNode: Int): List[Int] = {
    val distances: Array[Double] = Array.fill(graph.nrNodes)(Double.PositiveInfinity)
    distances(sourceNode - 1) = 0.0
    val nodes: MutableSet[Int] = (1 to graph.nrNodes).filterNot(_ == sourceNode).to(MutableSet)

    @tailrec
    def loop(node: Int, shortestDistToNode: Double): Unit = {
      val neighbours: List[Edge] = graph.neighbours(node)
      neighbours.foreach{
        case Edge(_, v, weight) =>
          val distanceThroughNode: Double = shortestDistToNode + weight
          if (distances(v - 1) > distanceThroughNode) distances(v - 1) = distanceThroughNode
      }
      if (nodes.nonEmpty) {
        val (u, distU): (Int, Double) = nodes.map(n => (n, distances(n - 1))).minBy{ case (_, dist) => dist }
        nodes.remove(u)
        loop(u, distU)
      }
    }

    loop(sourceNode, 0)
    distances.map(d => if (d.isPosInfinity) -1 else d.toInt).toList
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (nrNodes, nrEdges): (Int, Int) = readParameters(reader.next())
    val edges: List[Edge] = readEdges(reader, nrEdges)
    val graph = new DirectedGraph(nrNodes, edges)
    val result: List[Int] = calcShortestDistances(graph, sourceNode = 1)
    println(result.mkString(" "))
  }
}
