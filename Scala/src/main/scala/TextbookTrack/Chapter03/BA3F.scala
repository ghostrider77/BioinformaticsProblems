package TextbookTrack.Chapter03

object BA3F {
  import scala.annotation.tailrec

  class EulerianGraph(val adjacencyList: Map[Int, List[Int]]) {
    import EulerianGraph.{extendCycle, findNodeWithUnusedEdges, shiftCycle}

    def findEulerianCycle(): List[Int] = {
      @tailrec
      def loop(edges: Map[Int, List[Int]], cycle: List[Int]): List[Int] = {
        if (edges.isEmpty) cycle.reverse
        else {
          val (nextStartNode, ix): (Int, Int) = findNodeWithUnusedEdges(edges, cycle)
          val shiftedCycle: List[Int] = shiftCycle(cycle, ix, nextStartNode)
          val (remainingEdges, extendedCycle): (Map[Int, List[Int]], List[Int]) =
            extendCycle(edges, nextStartNode, shiftedCycle)
          loop(remainingEdges, extendedCycle)
        }
      }

      val startNode: Int = adjacencyList.keysIterator.min
      loop(adjacencyList, List(startNode))
    }
  }

  object EulerianGraph {
    private def findNodeWithUnusedEdges(adjacencyList: Map[Int, List[Int]], cycle: List[Int]): (Int, Int) =
      cycle.iterator.zipWithIndex.find { case (node, _) => adjacencyList.contains(node) } match {
        case None => throw new Exception("The cycle must contain nodes with unused outgoing edges.")
        case Some((node, ix)) => (node, ix)
      }

    private def shiftCycle(cycle: List[Int], ix: Int, startNode: Int): List[Int] = {
      val (first, second): (List[Int], List[Int]) = cycle.splitAt(ix + 1)
      startNode :: second ::: first.tail
    }

    private def extendCycle(adjacencyList: Map[Int, List[Int]],
                            startNode: Int,
                            initialCycle: List[Int]): (Map[Int, List[Int]], List[Int]) = {
      @tailrec
      def loop(edges: Map[Int, List[Int]], cycle: List[Int], currentNode: Int): (Map[Int, List[Int]], List[Int]) = {
        if (!edges.contains(currentNode)) (edges, cycle)
        else {
          edges.getOrElse(currentNode, Nil) match {
            case Nil => throw new Exception(s"The node $currentNode should have been removed earlier.")
            case nextNode :: rest =>
              val updatedEdges: Map[Int, List[Int]] =
                if (rest.isEmpty) edges.removed(currentNode) else edges.updated(currentNode, rest)
              loop(updatedEdges, nextNode :: cycle, nextNode)
          }
        }
      }

      loop(adjacencyList, initialCycle, startNode)
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
    val graph = new EulerianGraph(adjacencyList)
    val result: List[Int] = graph.findEulerianCycle()
    println(result.mkString("->"))
  }
}
