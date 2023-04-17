package AlgorithmicHeights

object SQ {
  final case class Edge(a: Int, b: Int)

  class Graph(val nrNodes: Int, edgeList: List[Edge]) {
    import Graph.buildAdjacencyList

    private val adjacencyList: Map[Int, Set[Int]] = buildAdjacencyList(edgeList)

    def neighbours(node: Int): Set[Int] = adjacencyList.getOrElse(node, Set.empty[Int])
  }

  object Graph {
    private def buildAdjacencyList(edgeList: List[Edge]): Map[Int, Set[Int]] =
      edgeList
        .flatMap{ case edge @ Edge(a, b) => List(edge, Edge(b, a)) }
        .groupMap{ case Edge(a, _) => a }{ case Edge(_, b) => b }
        .view
        .mapValues(_.toSet)
        .toMap
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

  private def hasCycleOfLengthFour(graph: Graph): Boolean = {
    (1 to graph.nrNodes).exists{
      node1 =>
        val neighbours1: Set[Int] = graph.neighbours(node1)
        if (neighbours1.isEmpty) false
        else (node1 + 1 to graph.nrNodes).exists{ node2 => graph.neighbours(node2).intersect(neighbours1).size >= 2 }
    }
  }

  def haveGraphsSquares(graphs: List[Graph]): List[Boolean] = graphs.map(hasCycleOfLengthFour)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrExamples: Int = reader.next().toInt
    val graphs: List[Graph] = readGraphs(reader, nrExamples)
    val result: List[Boolean] = haveGraphsSquares(graphs)
    println(result.map(if (_) "1" else "-1").mkString(" "))
  }
}
