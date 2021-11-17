package AlgorithmicHeights

object CC {
  import scala.annotation.tailrec

  private type Component = List[Int]
  final case class Edge(a: Int, b: Int)

  class Graph(val nrNodes: Int, edgeList: List[Edge]) {
    import Graph.buildAdjacencyList

    private val adjacencyList: Map[Int, List[Int]] = buildAdjacencyList(edgeList)

    lazy val connectedComponents: List[Component] = calcConnectedComponents()

    private def calcConnectedComponents(): List[Component] = {
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

      findComponents((1 to nrNodes).toList, Nil)
    }
  }

  object Graph {
    private def buildAdjacencyList(edgeList: List[Edge]): Map[Int, List[Int]] =
      edgeList
        .flatMap{ case edge @ Edge(a, b) => List(edge, Edge(b, a)) }
        .groupMap{ case Edge(a, _) => a }{ case Edge(_, b) => b }
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
    val graph = new Graph(nrNodes, edgeList)
    val result: Int = graph.connectedComponents.length
    println(result)
  }
}
