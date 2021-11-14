package AlgorithmicHeights

object DDEG {
  final case class Edge(a: Int, b: Int)

  class Graph(val nrNodes: Int, edgeList: List[Edge]) {
    import Graph.buildAdjacencyList

    private val adjacencyList: Map[Int, List[Int]] = buildAdjacencyList(edgeList)
    private val degrees: Map[Int, Int] = adjacencyList.map{ case (node, neighbours) => node -> neighbours.length }

    def neighbours(node: Int): List[Int] = adjacencyList.getOrElse(node, Nil)

    def degree(node: Int): Int = degrees.getOrElse(node, 0)
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

  def calcDoubleDegreeArray(nrNodes: Int, edgeList: List[Edge]): List[Int] = {
    val graph = new Graph(nrNodes, edgeList)
    (1 to nrNodes)
      .map(node => graph.neighbours(node).foldLeft(0)((acc, neighbour) => acc + graph.degree(neighbour)))
      .toList
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
    val result: List[Int] = calcDoubleDegreeArray(nrNodes, edgeList)
    println(result.mkString(" "))
  }
}
