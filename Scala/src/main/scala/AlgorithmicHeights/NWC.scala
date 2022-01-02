package AlgorithmicHeights

object NWC {
  import scala.annotation.tailrec

  final case class Edge(from: Int, to: Int, weight: Int)

  case class DirectedGraph(nrNodes: Int, edgeList: List[Edge])

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

  private def readGraphs(reader: Iterator[String], n: Int): List[DirectedGraph] = {
    (0 until n).map{ _ =>
      val (nrNodes, nrEdges): (Int, Int) = readParameters(reader.next())
      val edgeList: List[Edge] = readEdges(reader, nrEdges)
      DirectedGraph(nrNodes, edgeList)
    }.toList
  }

  def updateDistances(graph: DirectedGraph, distances: Array[Double]): Boolean = {
    @tailrec
    def loop(edges: List[Edge], isUpdated: Boolean): Boolean = edges match {
      case Nil => isUpdated
      case Edge(node, neighbour, weight) :: rest =>
        val distanceThroughNode: Double = distances(node - 1) + weight
        if (distances(neighbour - 1) > distanceThroughNode) {
          distances(neighbour - 1) = distanceThroughNode
          loop(rest, isUpdated = true)
        } else loop(rest, isUpdated)
    }

    loop(graph.edgeList, isUpdated = false)
  }

  def hasNegativeCycle(graph: DirectedGraph): Boolean = {
    val distances: Array[Double] = Array.fill(graph.nrNodes)(0.0)
    (0 until graph.nrNodes).foldLeft(false){ case (_, _) => updateDistances(graph, distances) }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrExamples: Int = reader.next().toInt
    val graphs: List[DirectedGraph] = readGraphs(reader, nrExamples)
    val result: List[Boolean] = graphs.map(hasNegativeCycle)
    println(result.map(if (_) "1" else "-1").mkString(" "))
  }
}
