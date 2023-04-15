package AlgorithmicHeights

object DAG {
  import scala.annotation.tailrec

  private type Component = List[Int]
  final case class Edge(a: Int, b: Int)
  final case class DFSResult(components: List[Component], previsitIds: Vector[Int], postvisitIds: Vector[Int])

  class DirectedGraph(val nrNodes: Int, edgeList: List[Edge]) {
    import DirectedGraph.buildAdjacencyList

    private val adjacencyList: Map[Int, List[Int]] = buildAdjacencyList(edgeList)

    lazy val isAcyclic: Boolean = {
      val DFSResult(_, _, postvisitIds) = depthFirstSearch()
      adjacencyList.forall{
        case (node, neighbours) =>
          val nodeNumber: Int = postvisitIds(node - 1)
          neighbours.forall(neighbour => postvisitIds(neighbour - 1) < nodeNumber)
      }
    }

    private def depthFirstSearch(): DFSResult = {
      val visitStarted: Array[Int] = Array.fill(nrNodes)(0)
      val visitEnded: Array[Int] = Array.fill(nrNodes)(0)
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
                traverseComponent(restOfStack, component)
            }
        }

        visitStarted(startingNode - 1) = previsitId.next()
        traverseComponent(List(startingNode), List(startingNode))
      }

      val connectedComponents: List[Component] = findComponents((1 to nrNodes).toList, Nil)
      DFSResult(connectedComponents, visitStarted.toVector, visitEnded.toVector)
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
      val _: String = reader.next()
      val (nrNodes, nrEdges): (Int, Int) = readParameters(reader.next())
      val edgeList: List[Edge] = readEdges(reader, nrEdges)
      new DirectedGraph(nrNodes, edgeList)
    }.toList
  }

  def testAcyclicity(graphs: List[DirectedGraph]): List[Boolean] = graphs.map(_.isAcyclic)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrExamples: Int = reader.next().toInt
    val graphs: List[DirectedGraph] = readGraphs(reader, nrExamples)
    val result: List[Boolean] = testAcyclicity(graphs)
    println(result.map(if (_) "1" else "-1").mkString(" "))
  }
}
