package TextbookTrack.Chapter03

object BA3I {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Map => MutableMap}

  class EulerianGraph[T](adjacencyList: Map[T, List[T]])(implicit ord: Ordering[T]) {
    import EulerianGraph.{extendCycle, findNodeWithUnusedEdges, shiftCycle}

    def findEulerianCycle(): List[T] = {
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

      val startNode: T = adjacencyList.keysIterator.min
      loop(adjacencyList, List(startNode))
    }
  }

  object EulerianGraph {
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
  }

  private def cartesianProduct[T](xs: Seq[T], n: Int): Iterator[List[T]] =
    (0 until n).foldLeft(Iterator(List.empty[T]))((acc, _) => acc.flatMap(ys => xs.map(_ :: ys)))

  private def createAdjacencyListFromBinaryStrings(k: Int): Map[String, List[String]] = {
    val binaryKMers: Iterator[String] = cartesianProduct(List('0', '1'), k).map(_.mkString)
    val adjacencyList: MutableMap[String, List[String]] = MutableMap.empty
    binaryKMers.foreach{ kMer =>
      val prefix: String = kMer.dropRight(1)
      val suffix: String = kMer.drop(1)
      val neighbours: List[String] = adjacencyList.getOrElse(prefix, Nil)
      adjacencyList.update(prefix, suffix :: neighbours)
    }
    adjacencyList.toMap
  }

  private def calcStringSpelledByAGenomePath(kMers: List[String]): String = kMers match {
    case Nil => ""
    case x :: xs => (x.toList ::: xs.map(_.last)).mkString
  }

  def findUniversalCircularBinaryString(k: Int): String = {
    if (k <= 0) ""
    else if (k == 1) "01"
    else {
      val adjacencyList: Map[String, List[String]] = createAdjacencyListFromBinaryStrings(k)
      val graph = new EulerianGraph(adjacencyList)
      val cycle: List[String] = graph.findEulerianCycle()
      val universalString: String = calcStringSpelledByAGenomePath(cycle)
      universalString.dropRight(k - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val k: Int = reader.next().toInt
    val result: String = findUniversalCircularBinaryString(k)
    println(result)
  }
}
