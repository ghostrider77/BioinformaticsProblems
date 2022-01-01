package AlgorithmicHeights

object HDAG {
  import scala.annotation.tailrec
  import scala.collection.mutable.ListBuffer

  private type Component = List[Int]
  final case class Edge(a: Int, b: Int)

  class DirectedGraph(val nrNodes: Int, edgeList: List[Edge]) {
    import DirectedGraph.buildAdjacencyList

    private val adjacencyList: Map[Int, List[Int]] = buildAdjacencyList(edgeList)

    lazy val topologicalSorting: List[Int] = depthFirstSearch()

    def hasEdge(node1: Int, node2: Int): Boolean = adjacencyList.getOrElse(node1, Nil).contains(node2)

    private def depthFirstSearch(): List[Int] = {
      val visitStarted: Array[Int] = Array.fill(nrNodes)(0)
      val visitEnded: Array[Int] = Array.fill(nrNodes)(0)
      val topologicalSorting: ListBuffer[Int] = ListBuffer()
      val previsitId: Iterator[Int] = Iterator.from(1)
      val postvisitId: Iterator[Int] = Iterator.from(1)

      def isNodeVisited(node: Int): Boolean = visitStarted(node - 1) > 0

      def findUnvisitedNeighbour(node: Int): Option[Int] =
        adjacencyList.getOrElse(node, Nil).find(!isNodeVisited(_))

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
    private def buildAdjacencyList(edgeList: List[Edge]): Map[Int, List[Int]] =
      edgeList.groupMap{ case Edge(a, _) => a }{ case Edge(_, b) => b }
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
      .collect{ case List(a, b) => Edge(a, b) }
      .toList

  private def readGraphs(reader: Iterator[String], n: Int): List[DirectedGraph] = {
    (0 until n).map{ _ =>
      val (nrNodes, nrEdges): (Int, Int) = readParameters(reader.next())
      val edgeList: List[Edge] = readEdges(reader, nrEdges)
      new DirectedGraph(nrNodes, edgeList)
    }.toList
  }

  private def createOutputString(result: Option[List[Int]]): String = result match {
    case None => "-1"
    case Some(path) => s"1 ${path.mkString(" ")}"
  }

  def findHamiltonianPath(graph: DirectedGraph): Option[List[Int]] = {
    val hasHamiltonianPath: Boolean =
      graph
        .topologicalSorting
        .sliding(2)
        .collect{ case List(node1, node2) => (node1, node2) }
        .forall{ case (node1, node2) => graph.hasEdge(node1, node2) }
    if (hasHamiltonianPath) Some(graph.topologicalSorting) else None
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrExamples: Int = reader.next().toInt
    val graphs: List[DirectedGraph] = readGraphs(reader, nrExamples)
    val results: List[Option[List[Int]]] = graphs.map(findHamiltonianPath)
    results.map(createOutputString).foreach(println)
  }
}
