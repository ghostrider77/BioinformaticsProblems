package AlgorithmicHeights

object CTE {
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

  private def readGraphs(reader: Iterator[String], n: Int): List[(DirectedGraph, Edge)] = {
    (0 until n).map{ _ =>
      val _: String = reader.next()
      val (nrNodes, nrEdges): (Int, Int) = readParameters(reader.next())
      val edgeList: List[Edge] = readEdges(reader, nrEdges)
      (new DirectedGraph(nrNodes, edgeList), edgeList.head)
    }.toList
  }

  private def calcShortestDistances(graph: DirectedGraph, sourceNode: Int): Vector[Double] = {
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
    distances.toVector
  }

  def calcShortestCycleThroughGivenEdge(graphs: List[(DirectedGraph, Edge)]): List[Int] =
    graphs.map{
      case (graph, Edge(nodeFrom, nodeTo, weight)) =>
        val distances: Vector[Double] = calcShortestDistances(graph, sourceNode = nodeTo)
        val dist: Double = distances(nodeFrom - 1)
        if (dist.isPosInfinity) -1 else dist.toInt + weight
    }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrExamples: Int = reader.next().toInt
    val graphs: List[(DirectedGraph, Edge)] = readGraphs(reader, nrExamples)
    val result: List[Int] = calcShortestCycleThroughGivenEdge(graphs)
    println(result.mkString(" "))
  }
}
