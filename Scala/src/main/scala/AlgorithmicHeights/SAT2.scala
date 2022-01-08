package AlgorithmicHeights

object SAT2 {
  import scala.annotation.tailrec
  import scala.collection.mutable.ListBuffer

  private type Component = List[Node]
  final case class DFSResult(components: List[Component],
                             topologicalOrdering: List[Node],
                             previsitIds: List[Int],
                             postvisitIds: List[Int])
  final case class Node(originalId: Int, nrNodes: Int) {
    override def toString: String = s"$originalId"

    val nodeId: Int = if (originalId < 0) -originalId + nrNodes else originalId

    def unary_- : Node = Node(-originalId, nrNodes)
  }
  final case class Edge(a: Node, b: Node)

  class DirectedGraph(val nrNodes: Int, val edgeList: List[Edge], nodeOrder: Option[List[Node]] = None) {
    import DirectedGraph.buildAdjacencyList

    val orderedNodes: List[Node] = nodeOrder match {
      case None => (1 to nrNodes).map(k => Node(originalId(k), nrNodes / 2)).toList
      case Some(nodes) => nodes
    }

    private val adjacencyList: Map[Node, List[Node]] = buildAdjacencyList(edgeList)

    def neighbours(node: Node): List[Node] = adjacencyList.getOrElse(node, Nil)

    private def originalId(nodeId: Int): Int = {
      val n: Int = nrNodes / 2
      if (nodeId <= n) nodeId else -(nodeId - n)
    }
  }

  object DirectedGraph {
    private def buildAdjacencyList(edgeList: List[Edge]): Map[Node, List[Node]] =
      edgeList.groupMap{ case Edge(a, _) => a }{ case Edge(_, b) => b }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(n, e) => (n, e)
    case _ => throw new Exception("Malformed input.")
  }

  private[AlgorithmicHeights] def readEdges(reader: Iterator[String], nrNodes: Int, nrEdges: Int): List[Edge] =
    reader
      .take(nrEdges)
      .map(convertToIntList)
      .collect{ case List(a, b) => (a, b) }
      .flatMap{
        case (a, b) =>
          val node1 = Node(a, nrNodes)
          val node2 = Node(b, nrNodes)
          List(Edge(-node1, node2), Edge(-node2, node1))
      }
      .toList

  private def readGraphs(reader: Iterator[String], n: Int): List[DirectedGraph] = {
    (0 until n).map{ _ =>
      val _: String = reader.next()
      val (nrNodes, nrEdges): (Int, Int) = readParameters(reader.next())
      val edgeList: List[Edge] = readEdges(reader, nrNodes, nrEdges)
      new DirectedGraph(2*nrNodes, edgeList)
    }.toList
  }

  private def depthFirstSearch(graph: DirectedGraph): DFSResult = {
    val visitStarted: Array[Int] = Array.fill(graph.nrNodes)(0)
    val visitEnded: Array[Int] = Array.fill(graph.nrNodes)(0)
    val topologicalSorting: ListBuffer[Node] = ListBuffer()
    val previsitId: Iterator[Int] = Iterator.from(1)
    val postvisitId: Iterator[Int] = Iterator.from(1)

    def isNodeVisited(node: Node): Boolean = visitStarted(node.nodeId - 1) > 0

    def findUnvisitedNeighbour(node: Node): Option[Node] = graph.neighbours(node).find(!isNodeVisited(_))

    def explore(startingNode: Node): Component = {
      @tailrec
      def traverseComponent(previsitStack: List[Node], component: Component): Component = previsitStack match {
        case Nil => component
        case node :: restOfStack =>
          findUnvisitedNeighbour(node) match {
            case Some(neighbour) =>
              visitStarted(neighbour.nodeId - 1) = previsitId.next()
              traverseComponent(neighbour :: previsitStack, neighbour :: component)
            case None =>
              visitEnded(node.nodeId - 1) = postvisitId.next()
              topologicalSorting += node
              traverseComponent(restOfStack, component)
          }
      }

      visitStarted(startingNode.nodeId - 1) = previsitId.next()
      traverseComponent(List(startingNode), List(startingNode))
    }

    @tailrec
    def findComponents(nodes: List[Node], components: List[Component]): List[Component] = nodes match {
      case Nil => components
      case node :: remainingNodes =>
        if (isNodeVisited(node)) findComponents(remainingNodes, components)
        else {
          val currentComponent: Component = explore(node)
          findComponents(remainingNodes, currentComponent :: components)
        }
    }

    val components: List[Component] = findComponents(graph.orderedNodes, Nil)
    DFSResult(components, topologicalSorting.reverseIterator.toList, visitStarted.toList, visitEnded.toList)
  }

  private def createGraphWithEdgesReversed(graph: DirectedGraph, postvisitIds: List[Int]): DirectedGraph = {
    val reversedEdges: List[Edge] = graph.edgeList.map{ case Edge(from, to) => Edge(to, from) }
    val nodeOrder: List[Node] =
      graph.orderedNodes
        .zip(postvisitIds)
        .sortBy{ case (_, postvisitId) => postvisitId }(Ordering[Int].reverse)
        .map{ case (node, _) => node }
    new DirectedGraph(graph.nrNodes, reversedEdges, Some(nodeOrder))
  }

  private def calcStronglyConnectedComponents(graph: DirectedGraph): List[Component] = {
    val DFSResult(_, _, _, postvisitNumbers) = depthFirstSearch(graph)
    val reversedGraph: DirectedGraph = createGraphWithEdgesReversed(graph, postvisitNumbers)
    val DFSResult(components, _, _, _) = depthFirstSearch(reversedGraph)
    components
  }

  private def calcComponentAssignment(component: Component, literalAssignment: Set[Node]): Option[Set[Node]] = {
    @tailrec
    def loop(acc: Set[Node], nodes: List[Node]): Option[Set[Node]] = nodes match {
      case Nil => Some(acc)
      case node :: rest =>
        if (acc.contains(-node)) None
        else if (!literalAssignment.contains(node) && !literalAssignment.contains(-node)) loop(acc + node, rest)
        else loop(acc, rest)
    }

    loop(Set.empty[Node], component)
  }

  def solve2SATProblem(graph: DirectedGraph): Set[Node] = {
    @tailrec
    def loop(acc: Set[Node], cs: List[Component]): Set[Node] = cs match {
      case Nil => acc
      case component :: css =>
        calcComponentAssignment(component, acc) match {
          case None => Set()
          case Some(componentAssignment) => loop(acc ++ componentAssignment, css)
        }
    }

    val components: List[Component] = calcStronglyConnectedComponents(graph)
    loop(Set.empty[Node], components)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrExamples: Int = reader.next().toInt
    val graphs: List[DirectedGraph] = readGraphs(reader, nrExamples)
    val result: List[Set[Node]] = graphs.map(solve2SATProblem)
    result.foreach{
      line =>
        if (line.isEmpty) println("0")
        else {
          val sortedResult: List[Node] = line.toList.sortBy{ node => math.abs(node.originalId) }
          println(s"1 ${sortedResult.mkString(" ")}")
        }
    }
  }
}
