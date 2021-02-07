package TextbookTrack.Chapter03

object BA3H {
  import scala.annotation.tailrec

  class DeBruijnGraph[T](adjacencyList: Map[T, List[T]])(implicit ord: Ordering[T]) {
    import DeBruijnGraph._

    def findEulerianPath(): List[T] = {
      @tailrec
      def loop(edges: Map[T, List[T]], cycle: List[T]): List[T] = {
        if (edges.isEmpty) cycle.reverse
        else {
          val (nextStartNode, ix): (T, Int) = findNodeWithUnusedEdges(edges, cycle)
          val shiftedCycle: List[T] = shiftCycle(cycle, ix, nextStartNode)
          val (remainingEdges, extendedCycle): (Map[T, List[T]], List[T]) =
            extendCycle(edges, nextStartNode, shiftedCycle)
          loop(remainingEdges, extendedCycle)
        }
      }

      findEulerianPathEndpoints(adjacencyList) match {
        case None => throw new Exception("The graph does not have an Eulerian path.")
        case Some((pathStart, pathEnd)) =>
          val extendedEdges: Map[T, List[T]] = addExtraEdge(adjacencyList, pathStart, pathEnd)
          val startNode: T = extendedEdges.keys.min
          val cycle: List[T] = loop(extendedEdges, List(startNode))
          removeExtraEdgeFromCycle(cycle, pathStart, pathEnd)
      }
    }
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

    private def findNodeWithUnusedEdges[T](adjacencyList: Map[T, List[T]], cycle: List[T]): (T, Int) =
      cycle.iterator.zipWithIndex.find { case (node, _) => adjacencyList.contains(node) } match {
        case None => throw new Exception("The cycle must contain nodes with unused outgoing edges.")
        case Some((node, ix)) => (node, ix)
      }

    private def shiftCycle[T](cycle: List[T], ix: Int, startNode: T): List[T] = {
      val (first, second): (List[T], List[T]) = cycle.splitAt(ix + 1)
      startNode :: second ::: first.tail
    }

    private def extendCycle[T](adjacencyList: Map[T, List[T]],
                               startNode: T,
                               initialCycle: List[T]): (Map[T, List[T]], List[T]) = {
      @tailrec
      def loop(edges: Map[T, List[T]], cycle: List[T], currentNode: T): (Map[T, List[T]], List[T]) = {
        if (!edges.contains(currentNode)) (edges, cycle)
        else {
          edges.getOrElse(currentNode, Nil) match {
            case Nil => throw new Exception(s"The node $currentNode should have been removed earlier.")
            case nextNode :: rest =>
              val updatedEdges: Map[T, List[T]] =
                if (rest.isEmpty) edges.removed(currentNode) else edges.updated(currentNode, rest)
              loop(updatedEdges, nextNode :: cycle, nextNode)
          }
        }
      }

      loop(adjacencyList, initialCycle, startNode)
    }

    private def calcDegrees[T](adjacencyList: Map[T, List[T]]): Map[T, Int] =
      adjacencyList
        .iterator
        .flatMap{ case (node, neighbours) => neighbours.flatMap(neighbour => List((node, 1), (neighbour, -1)))}
        .toList
        .groupBy{ case (node, _) => node }
        .view
        .mapValues(_.map{ case (_, degree) => degree }.sum)
        .toMap

    private def findEulerianPathEndpoints[T](adjacencyList: Map[T, List[T]]): Option[(T, T)] = {
      val degrees: Map[T, Int] = calcDegrees(adjacencyList)
      def findNodeWithGivenDegree(value: Int): Option[T] =
        degrees.find{ case (_, degree) => degree == value }.map{ case (node, _) => node }

      val pathStart: Option[T] = findNodeWithGivenDegree(1)
      val pathEnd: Option[T] = findNodeWithGivenDegree(-1)
      for {
        start <- pathStart
        end <- pathEnd
      } yield (start, end)
    }

    private def addExtraEdge[T](adjacencyList: Map[T, List[T]], pathStart: T, pathEnd: T): Map[T, List[T]] = {
      val neighbours: List[T] = adjacencyList.getOrElse(pathEnd, Nil)
      adjacencyList.updated(pathEnd, pathStart :: neighbours)
    }

    private def removeExtraEdgeFromCycle[T](cycle: List[T], pathStart: T, pathEnd: T): List[T] = {
      val indexOfExtraEdge: List[Int] = (for {
        (List(a, b), ix) <- cycle.sliding(2).zipWithIndex
        if a == pathEnd && b == pathStart
      } yield ix).toList

      indexOfExtraEdge match {
        case ix :: _ => cycle.drop(ix + 1) ::: cycle.slice(1, ix + 1)
        case _ => throw new Exception("Extra edge appeared more than once or not at all.")
      }
    }
  }

  def calcStringSpelledByAGenomePath(kMers: List[String]): String = kMers match {
    case Nil => ""
    case x :: xs => (x.toList ::: xs.map(_.last)).mkString
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val _: Int = reader.next().toInt
    val kMers: List[String] = reader.toList
    val graph = DeBruijnGraph(kMers)
    val path: List[String] = graph.findEulerianPath()
    val result: String = calcStringSpelledByAGenomePath(path)
    println(result)
  }
}
