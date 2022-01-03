package AlgorithmicHeights

object SC {
  import scala.annotation.tailrec
  import scala.collection.mutable.ListBuffer

  private type Component = List[Int]
  final case class Edge(a: Int, b: Int)
  final case class DFSResult(components: List[Component],
                             topologicalOrdering: List[Int],
                             previsitIds: List[Int],
                             postvisitIds: List[Int])

  class DirectedGraph(val nrNodes: Int, val edgeList: List[Edge], nodeOrder: Option[List[Int]] = None) {
    import DirectedGraph.buildAdjacencyList

    val orderedNodes: List[Int] = nodeOrder match {
      case None => (1 to nrNodes).toList
      case Some(nodes) => nodes
    }

    private val adjacencyList: Map[Int, List[Int]] = buildAdjacencyList(edgeList)

    def neighbours(node: Int): List[Int] = adjacencyList.getOrElse(node, Nil)
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
      val _: String = reader.next()
      val (nrNodes, nrEdges): (Int, Int) = readParameters(reader.next())
      val edgeList: List[Edge] = readEdges(reader, nrEdges)
      new DirectedGraph(nrNodes, edgeList)
    }.toList
  }

  private def depthFirstSearch(graph: DirectedGraph): DFSResult = {
    val visitStarted: Array[Int] = Array.fill(graph.nrNodes)(0)
    val visitEnded: Array[Int] = Array.fill(graph.nrNodes)(0)
    val topologicalSorting: ListBuffer[Int] = ListBuffer()
    val previsitId: Iterator[Int] = Iterator.from(1)
    val postvisitId: Iterator[Int] = Iterator.from(1)

    def isNodeVisited(node: Int): Boolean = visitStarted(node - 1) > 0

    def findUnvisitedNeighbour(node: Int): Option[Int] = graph.neighbours(node).find(!isNodeVisited(_))

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

    val components: List[Component] = findComponents(graph.orderedNodes, Nil)
    DFSResult(components.reverse, topologicalSorting.reverseIterator.toList, visitStarted.toList, visitEnded.toList)
  }

  private def createGraphWithEdgesReversed(graph: DirectedGraph, postvisitIds: List[Int]): DirectedGraph = {
    val reversedEdges: List[Edge] = graph.edgeList.map{ case Edge(from, to) => Edge(to, from) }
    val nodeOrder: List[Int] =
      (1 to graph.nrNodes)
        .zip(postvisitIds)
        .sortBy{ case (_, postvisitId) => postvisitId }(Ordering[Int].reverse)
        .map{ case (id, _) => id }
        .toList
    new DirectedGraph(graph.nrNodes, reversedEdges, Some(nodeOrder))
  }

  private def hasEdgeFromSourceToNextComponent(source: Set[Int], next: Set[Int], graph: DirectedGraph): Boolean =
    source.flatMap(graph.neighbours).exists(next.contains)

  def isSemiConnected(graph: DirectedGraph): Boolean = {
    val DFSResult(_, _, _, postvisitNumbers) = depthFirstSearch(graph)
    val reversedGraph: DirectedGraph = createGraphWithEdgesReversed(graph, postvisitNumbers)
    val DFSResult(components, _, _, _) = depthFirstSearch(reversedGraph)
    components
      .iterator
      .map(_.toSet)
      .sliding(2)
      .map(_.toList)
      .collect{ case List(source, next) => (source, next) }
      .forall{ case (source, next) => hasEdgeFromSourceToNextComponent(source, next, graph) }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrExamples: Int = reader.next().toInt
    val graphs: List[DirectedGraph] = readGraphs(reader, nrExamples)
    val result: List[Boolean] = graphs.map(isSemiConnected)
    println(result.map(if (_) 1 else -1).mkString(" "))
  }
}
