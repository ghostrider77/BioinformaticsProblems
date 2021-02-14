package TextbookTrack.Chapter03

object BA3M {
  import scala.annotation.tailrec

  class Graph[T](adjacencyList: Map[T, List[T]]) {
    private val (inDegrees, outDegrees): (Map[T, Int], Map[T, Int]) = calcNodeDegrees()

    lazy val nodes: Set[T] = adjacencyList.keySet ++ adjacencyList.valuesIterator.flatMap(_.toSet).toSet

    private def calcNodeDegrees(): (Map[T, Int], Map[T, Int]) = {
      val outDegrees: Map[T, Int] = adjacencyList.map{ case (node, neighbours) => (node, neighbours.length)}
      val inDegrees: Map[T, Int] =
        adjacencyList
          .iterator
          .flatMap{ case (node, neighbours) => neighbours.map(neighbour => (neighbour, node))}
          .toList
          .groupBy{ case (neighbour, _) => neighbour }
          .view
          .mapValues(_.length)
          .toMap
      (inDegrees, outDegrees)
    }

    def neighbours(node: T): List[T] = adjacencyList.getOrElse(node, Nil)

    def inDegree(node: T): Int = inDegrees.getOrElse(node, 0)

    def outDegree(node: T): Int = outDegrees.getOrElse(node, 0)

    def isNodeOneInOneOut(node: T): Boolean = (inDegree(node) == 1) && (outDegree(node) == 1)

    def nodesWithOutgoingEdges: Set[T] = nodes.filter(outDegree(_) > 0)
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

  private def buildNonBranchingPaths[T](graph: Graph[T], node: T): List[List[T]] = {
    def extendPath(node: T, neighbour: T): List[T] = {
      @tailrec
      def loop(path: List[T], w: T): List[T] = {
        if (graph.isNodeOneInOneOut(w)) {
          graph.neighbours(w) match {
            case List(x) => loop(x :: path, x)
            case _ => throw new Exception("Node should have only one neighbour.")
          }
        } else path.reverse
      }
      loop(List(neighbour, node), neighbour)
    }

    graph.neighbours(node).map(neighbour => extendPath(node, neighbour))
  }

  private def buildCycle[T](graph: Graph[T], startNode: T): List[T] = {
    @tailrec
    def loop(cycle: List[T], node: T): List[T] = {
      graph.neighbours(node) match {
        case List(nextNode) =>
          if (nextNode != startNode) loop(nextNode :: cycle, nextNode)
          else (startNode :: cycle).reverse
        case _ => throw new Exception("Node should have only one neighbour.")
      }
    }
    loop(List(startNode), startNode)
  }

  private def findIsolatedCyclesInGraph[T](graph: Graph[T], paths: Set[List[T]]): Set[List[T]] = {
    @tailrec
    def loop(isolatedCycles: Set[List[T]], unusedNodes: Set[T]): Set[List[T]] = {
      if (unusedNodes.isEmpty) isolatedCycles
      else {
        val node: T = unusedNodes.head
        val cycle: List[T] = buildCycle(graph, node)
        loop(isolatedCycles + cycle, unusedNodes -- cycle)
      }
    }
    val missingNodes: Set[T] = graph.nodesWithOutgoingEdges.diff(paths.flatten)
    loop(Set.empty[List[T]], missingNodes)
  }

  def findMaximalNonBranchingPaths[T](graph: Graph[T]): Set[List[T]] = {
    val paths: Set[List[T]] =
      graph.nodes
        .withFilter(node => !graph.isNodeOneInOneOut(node) && graph.outDegree(node) > 0)
        .flatMap(buildNonBranchingPaths(graph, _))
    val cycles: Set[List[T]] = findIsolatedCyclesInGraph(graph, paths)
    paths ++ cycles
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val adjacencyList: Map[Int, List[Int]] = readLines(reader)
    val graph = new Graph[Int](adjacencyList)
    val result: Set[List[Int]] = findMaximalNonBranchingPaths(graph)
    result.foreach(line => println(line.mkString(" -> ")))
  }
}
