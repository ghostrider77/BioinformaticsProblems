package TextbookTrack.Chapter05

object BA5N {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Map => MutableMap}

  class DirectedGraph[T](val adjacencyList: Map[T, List[T]]) {
    import DirectedGraph.depthFirstSearch

    private val nodes: Set[T] = adjacencyList.keySet ++ adjacencyList.valuesIterator.flatMap(_.toSet).toSet

    lazy val topologicalSorting: List[T] = depthFirstSearch(this)
  }

  object DirectedGraph {
    private def depthFirstSearch[T](graph: DirectedGraph[T]): List[T] = {
      val visitStarted: MutableMap[T, Int] = MutableMap()
      val previsitId: Iterator[Int] = Iterator.from(1)

      def isNodeVisited(node: T): Boolean = visitStarted.getOrElse(node, 0) > 0

      def findUnvisitedNeighbour(node: T): Option[T] =
        graph.adjacencyList.getOrElse(node, Nil).find(!isNodeVisited(_))

      def explore(startingNode: T, topologicalSorting: List[T]): List[T] = {
        @tailrec
        def loop(acc: List[T], previsitStack: List[T]): List[T] = previsitStack match {
          case Nil => acc
          case lastNode :: rest =>
            findUnvisitedNeighbour(lastNode) match {
              case None => loop(lastNode :: acc, rest)
              case Some(unvisitedNeighbour) =>
                visitStarted(unvisitedNeighbour) = previsitId.next()
                loop(acc, unvisitedNeighbour :: previsitStack)
          }
        }

        visitStarted(startingNode) = previsitId.next()
        loop(topologicalSorting, List(startingNode))
      }

      @tailrec
      def run(topologicalSorting: List[T], nodes: Set[T]): List[T] = {
        nodes.headOption match {
          case None => topologicalSorting
          case Some(node) =>
            val updatedSorting: List[T] =
              if (isNodeVisited(node)) topologicalSorting else explore(node, topologicalSorting)
            run(updatedSorting, nodes.tail)
        }
      }
      run(Nil, graph.nodes)
    }
  }

  private def splitEdges(line: String): (String, String) = line.split(" -> ").toList match {
    case List(node, neighbours) => (node, neighbours)
    case _ => throw new Exception("Malformed input data.")
  }

  def readLines(lines: Iterator[String]): Map[Int, List[Int]] =
    lines.map { line =>
      val (node, neighbours): (String, String) = splitEdges(line)
      node.toInt -> neighbours.split(",").map(_.toInt).toList
    }.toMap

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val adjacencyList: Map[Int, List[Int]] = readLines(reader)
    val graph = new DirectedGraph[Int](adjacencyList)
    val result: List[Int] = graph.topologicalSorting
    println(result.mkString(", "))
  }
}
