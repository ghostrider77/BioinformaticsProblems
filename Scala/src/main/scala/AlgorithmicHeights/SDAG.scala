package AlgorithmicHeights

object SDAG {
  import scala.annotation.tailrec
  import scala.collection.mutable.ListBuffer

  private type Component = List[Int]
  final case class Edge(from: Int, to: Int, weight: Int)

  class DirectedGraph(val nrNodes: Int, edgeList: List[Edge]) {
    import DirectedGraph.buildAdjacencyList

    private val adjacencyList: Map[Int, List[Edge]] = buildAdjacencyList(edgeList)

    lazy val topologicalSorting: List[Int] = depthFirstSearch()

    def neighbours(node: Int): List[Edge] = adjacencyList.getOrElse(node, Nil)

    private def depthFirstSearch(): List[Int] = {
      val visitStarted: Array[Int] = Array.fill(nrNodes)(0)
      val visitEnded: Array[Int] = Array.fill(nrNodes)(0)
      val topologicalSorting: ListBuffer[Int] = ListBuffer()
      val previsitId: Iterator[Int] = Iterator.from(1)
      val postvisitId: Iterator[Int] = Iterator.from(1)

      def isNodeVisited(node: Int): Boolean = visitStarted(node - 1) > 0

      def findUnvisitedNeighbour(node: Int): Option[Int] =
        neighbours(node).find{ case Edge(_, neighbour, _) => !isNodeVisited(neighbour) }.map(_.to)

      @tailrec
      def findComponents(nodes: List[Int], components: List[Component]): List[Component] = nodes match {
        case Nil => components
        case node :: remainingNodes =>
          if (isNodeVisited(node)) findComponents(remainingNodes, components)
          else {
            val currentComponent: Component = explore(node)
            findComponents(remainingNodes, currentComponent :: components)
          }
      }

      def explore(startingNode: Int): Component = {
        @tailrec
        def traverseComponent(previsitStack: List[Int], component: Component): Component = previsitStack match {
          case Nil => component
          case node :: restOfStack =>
            findUnvisitedNeighbour(node) match {
              case Some(neighbour) =>
                visitStarted(neighbour - 1) = previsitId.next()
                traverseComponent(neighbour :: previsitStack, neighbour :: component)
              case None =>
                visitEnded(node - 1) = postvisitId.next()
                topologicalSorting += node
                traverseComponent(restOfStack, component)
            }
        }

        visitStarted(startingNode - 1) = previsitId.next()
        traverseComponent(List(startingNode), List(startingNode))
      }

      val _: List[Component] = findComponents((1 to nrNodes).toList, Nil)
      topologicalSorting.reverseIterator.toList
    }
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

  def findShortestPath(graph: DirectedGraph, startNode: Int): Vector[Double] = {
    val distances: Array[Double] = Array.fill(graph.nrNodes)(Double.PositiveInfinity)
    distances(startNode - 1) = 0.0
    graph.topologicalSorting.dropWhile(_ != startNode).foreach{
      node =>
        val shortestPathToNode: Double = distances(node - 1)
        graph.neighbours(node).foreach{
          case Edge(_, neighbour, weight) =>
            if (distances(neighbour - 1) > shortestPathToNode + weight) {
              distances(neighbour - 1) = shortestPathToNode + weight
            }
        }
    }
    distances.toVector
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (nrNodes, nrEdges): (Int, Int) = readParameters(reader.next())
    val edgeList: List[Edge] = readEdges(reader, nrEdges)
    val graph = new DirectedGraph(nrNodes, edgeList)
    val result: Vector[Double] = findShortestPath(graph, startNode = 1)
    println(result.map(x => if (x.isPosInfinity) "x" else x.toInt.toString).mkString(" "))
  }
}
