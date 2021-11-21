package AlgorithmicHeights

object BIP {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Queue => MutableQueue}

  sealed trait NodeColor
  final case object Red extends NodeColor
  final case object Blue extends NodeColor

  final case class Edge(a: Int, b: Int)

  class Graph(val nrNodes: Int, edgeList: List[Edge]) {
    import Graph.buildAdjacencyList

    private val adjacencyList: Map[Int, List[Int]] = buildAdjacencyList(edgeList)

    def neighbours(node: Int): List[Int] = adjacencyList.getOrElse(node, Nil)
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

  private def readEdges(reader: Iterator[String], nrEdges: Int): List[Edge] =
    reader
      .take(nrEdges)
      .map(convertToIntList)
      .collect{ case List(a, b) => Edge(a, b) }
      .toList

  private def readGraphs(reader: Iterator[String], n: Int): List[Graph] = {
    (0 until n).map{ _ =>
      val _: String = reader.next()
      val (nrNodes, nrEdges): (Int, Int) = readParameters(reader.next())
      val edgeList: List[Edge] = readEdges(reader, nrEdges)
      new Graph(nrNodes, edgeList)
    }.toList
  }

  private def getOppositeColor(color: NodeColor) = color match {
    case Red => Blue
    case Blue => Red
  }

  private def findConsistentComponentColoring(graph: Graph, startNode: Int): Option[Map[Int, NodeColor]] = {
    val queue: MutableQueue[Int] = MutableQueue(startNode)

    @tailrec
    def loop(nodeColorsInComponent: Map[Int, NodeColor]): Option[Map[Int, NodeColor]] = {
      if (queue.isEmpty) Some(nodeColorsInComponent)
      else {
        val node: Int = queue.dequeue()
        val nodeColor: NodeColor = nodeColorsInComponent(node)
        val oppositeColor: NodeColor = getOppositeColor(nodeColor)
        val neighbours: List[Int] = graph.neighbours(node)
        val (coloredNeighbours, uncoloredNeighbours): (List[Int], List[Int]) =
          neighbours.partition(nodeColorsInComponent.contains)
        if (coloredNeighbours.map(nodeColorsInComponent(_)).contains(nodeColor)) None
        else {
          uncoloredNeighbours.foreach(queue.enqueue)
          loop(nodeColorsInComponent ++ uncoloredNeighbours.map(neighbour => neighbour -> oppositeColor))
        }
      }
    }

    loop(Map(startNode -> Red))
  }

  private def isBipartite(graph: Graph): Boolean = {
    @tailrec
    def loop(nodeColors: Map[Int, NodeColor], startNode: Int): Boolean = {
      if (startNode >= graph.nrNodes) true
      else if (!nodeColors.contains(startNode)) {
        findConsistentComponentColoring(graph, startNode) match {
          case None => false
          case Some(componentColoring) => loop(nodeColors ++ componentColoring, startNode + 1)
        }
      }
      else loop(nodeColors, startNode + 1)
    }

    loop(Map(), startNode = 1)
  }

  def testBipartiteness(graphs: List[Graph]): List[Boolean] = graphs.map(isBipartite)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrExamples: Int = reader.next().toInt
    val graphs: List[Graph] = readGraphs(reader, nrExamples)
    val result: List[Boolean] = testBipartiteness(graphs)
    println(result.map(if (_) "1" else "-1"))
  }
}
