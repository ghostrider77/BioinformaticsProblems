package TextbookTrack.Chapter05

object BA5D {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Map => MutableMap}

  final case class WeightedEdge(nodeFrom: Int, nodeTo: Int, weight: Int)

  class DirectedWeightedGraph(val adjacencyList: Map[Int, List[WeightedEdge]]) {
    import DirectedWeightedGraph.depthFirstSearch

    private val nodes: Set[Int] = adjacencyList.keySet ++ adjacencyList.valuesIterator.flatMap(_.map(_.nodeTo)).toSet

    val incomingEdges: Map[Int, List[WeightedEdge]] =
      (for {
        (node, neighbours) <- adjacencyList.iterator
        WeightedEdge(_, nodeTo, weight) <- neighbours
      } yield WeightedEdge(nodeTo, node, weight)).toList.groupBy(_.nodeFrom)

    lazy val topologicalSorting: List[Int] = depthFirstSearch(this)
  }

  object DirectedWeightedGraph {
    private def depthFirstSearch(graph: DirectedWeightedGraph): List[Int] = {
      val visitStarted: MutableMap[Int, Int] = MutableMap()
      val previsitId: Iterator[Int] = Iterator.from(1)

      def isNodeVisited(node: Int): Boolean = visitStarted.getOrElse(node, 0) > 0

      def findUnvisitedNeighbour(node: Int): Option[Int] =
        graph.adjacencyList.getOrElse(node, Nil).map(_.nodeTo).find(!isNodeVisited(_))

      def explore(startingNode: Int, topologicalSorting: List[Int]): List[Int] = {
        @tailrec
        def loop(acc: List[Int], previsitStack: List[Int]): List[Int] = previsitStack match {
          case Nil => acc
          case lastNode :: rest =>
            findUnvisitedNeighbour(lastNode) match {
              case None => loop(lastNode :: acc, rest)
              case Some(unvistedNeighbour) =>
                visitStarted(unvistedNeighbour) = previsitId.next()
                loop(acc, unvistedNeighbour :: previsitStack)
            }
        }

        visitStarted(startingNode) = previsitId.next()
        loop(topologicalSorting, List(startingNode))
      }

      @tailrec
      def run(topologicalSorting: List[Int], nodes: Set[Int]): List[Int] = {
        nodes.headOption match {
          case None => topologicalSorting
          case Some(node) =>
            val updatedSorting: List[Int] =
              if (isNodeVisited(node)) topologicalSorting else explore(node, topologicalSorting)
            run(updatedSorting, nodes.tail)
        }
      }
      run(Nil, graph.nodes)
    }
  }

  private def splitEdge(line: String): WeightedEdge = line.replace("->", ":").split(":").map(_.toInt).toList match {
    case List(from, to, weight) => WeightedEdge(from, to, weight)
    case _ => throw new Exception("Malformed input data.")
  }

  private def collectLongestPath(backtrack: Map[Int, Int], source: Int, sink: Int): List[Int] = {
    @tailrec
    def loop(acc: List[Int], currentNode: Int): List[Int] = {
      if (currentNode == source) acc
      else {
        val nextNode: Int = backtrack(currentNode)
        loop(nextNode :: acc, nextNode)
      }
    }
    loop(List(sink), sink)
  }

  def readWeightedEdges(lines: Iterator[String]): Map[Int, List[WeightedEdge]] =
    lines.map(splitEdge).toList.groupBy(_.nodeFrom)

  def findLongestPath(graph: DirectedWeightedGraph, source: Int, sink: Int): (Int, List[Int]) = {
    @tailrec
    def loop(longestPath: Map[Int, Double],
             backtrack: Map[Int, Int],
             orderedNodes: List[Int]): (Map[Int, Double], Map[Int, Int]) = orderedNodes match {
      case Nil => (longestPath, backtrack)
      case node :: rest =>
        graph.incomingEdges.get(node) match {
          case Some(predecessors) if node != source =>
            val paths: List[(Int, Double)] = predecessors.map{
              case WeightedEdge(_, u, w) => (u, longestPath.getOrElse(u, Double.NegativeInfinity) + w)
            }
            val (v, s): (Int, Double) = paths.maxBy{ case (_, cost) => cost }
            loop(longestPath + (node -> s), backtrack + (node -> v), rest)
          case _ => loop(longestPath, backtrack, rest)
        }
    }
    val (longestPath, backtrack): (Map[Int, Double], Map[Int, Int]) =
      loop(Map(source -> 0), Map(), graph.topologicalSorting)
    (longestPath(sink).toInt, collectLongestPath(backtrack, source, sink))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val source: Int = reader.next().toInt
    val sink: Int = reader.next().toInt
    val adjacencyList: Map[Int, List[WeightedEdge]] = readWeightedEdges(reader)
    val graph = new DirectedWeightedGraph(adjacencyList)
    val (length, longestPath): (Int, List[Int]) = findLongestPath(graph, source, sink)
    println(length)
    println(longestPath.mkString("->"))
  }
}
