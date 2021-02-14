package TextbookTrack.Chapter03

object BA3K {
  import scala.annotation.tailrec

  class DeBruijnGraph[T](adjacencyList: Map[T, List[T]]) {
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

  object DeBruijnGraph {
    def apply(kMers: List[String]): DeBruijnGraph[String] = {
      val adjacencyList: Map[String, List[String]] =
        kMers
          .map(kMer => (kMer.dropRight(1), kMer.drop(1)))
          .groupBy{ case (prefix, _) => prefix }
          .view
          .mapValues(_.map{ case (_, suffix) => suffix })
          .toMap
      new DeBruijnGraph[String](adjacencyList)
    }
  }

  private def buildNonBranchingPaths[T](graph: DeBruijnGraph[T], node: T): List[List[T]] = {
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

  private def buildCycle[T](graph: DeBruijnGraph[T], startNode: T): List[T] = {
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

  private def findIsolatedCyclesInGraph[T](graph: DeBruijnGraph[T], paths: List[List[T]]): Set[List[T]] = {
    @tailrec
    def loop(isolatedCycles: Set[List[T]], unusedNodes: Set[T]): Set[List[T]] = {
      if (unusedNodes.isEmpty) isolatedCycles
      else {
        val node: T = unusedNodes.head
        val cycle: List[T] = buildCycle(graph, node)
        loop(isolatedCycles + cycle, unusedNodes -- cycle)
      }
    }
    val missingNodes: Set[T] = graph.nodesWithOutgoingEdges.diff(paths.flatten.toSet)
    loop(Set.empty[List[T]], missingNodes)
  }

  def findMaximalNonBranchingPaths[T](graph: DeBruijnGraph[T]): List[List[T]] = {
    val paths: List[List[T]] =
      graph.nodes
        .iterator
        .filter(node => !graph.isNodeOneInOneOut(node) && graph.outDegree(node) > 0)
        .flatMap(buildNonBranchingPaths(graph, _))
        .toList
    val cycles: Set[List[T]] = findIsolatedCyclesInGraph(graph, paths)
    paths ++ cycles
  }

  def calcStringSpelledByAGenomePath(kMers: List[String]): String = kMers match {
    case Nil => ""
    case x :: xs => (x.toList ::: xs.map(_.last)).mkString
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val kMers: List[String] = reader.toList
    val graph = DeBruijnGraph(kMers)
    val result: List[List[String]] = findMaximalNonBranchingPaths(graph)
    println(result.map(calcStringSpelledByAGenomePath).mkString(" "))
  }
}
