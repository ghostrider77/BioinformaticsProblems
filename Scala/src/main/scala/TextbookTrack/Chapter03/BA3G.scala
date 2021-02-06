package TextbookTrack.Chapter03

object BA3G {
  import scala.annotation.tailrec

  class EulerianGraph(val adjacencyList: Map[Int, List[Int]]) {
    import EulerianGraph.{extendCycle, findNodeWithUnusedEdges, shiftCycle}

    def findEulerianPath(): List[Int] = {
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

      findEulerianPathEndpoints(adjacencyList) match {
        case None => throw new Exception("The graph does not have an Eulerian path.")
        case Some((pathStart, pathEnd)) =>
          val extendedEdges: Map[Int, List[Int]] = addExtraEdge(adjacencyList, pathStart, pathEnd)
          val startNode: Int = extendedEdges.keysIterator.min
          val cycle: List[Int] = loop(extendedEdges, List(startNode))
          removeExtraEdgeFromCycle(cycle, pathStart, pathEnd)
      }
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

  private def calcDegrees(adjacencyList: Map[Int, List[Int]]): Map[Int, Int] =
    adjacencyList
      .iterator
      .flatMap{ case (node, neighbours) => neighbours.flatMap(neighbour => List((node, 1), (neighbour, -1)))}
      .toList
      .groupBy{ case (node, _) => node }
      .view
      .mapValues(_.map{ case (_, degree) => degree }.sum)
      .toMap

  private def findEulerianPathEndpoints(adjacencyList: Map[Int, List[Int]]): Option[(Int, Int)] = {
    val degrees: Map[Int, Int] = calcDegrees(adjacencyList)
    def findNodeWithGivenDegree(value: Int): Option[Int] =
      degrees.find{ case (_, degree) => degree == value }.map{ case (node, _) => node }

    val pathStart: Option[Int] = findNodeWithGivenDegree(1)
    val pathEnd: Option[Int] = findNodeWithGivenDegree(-1)
    for {
      start <- pathStart
      end <- pathEnd
    } yield (start, end)
  }

  private def addExtraEdge(adjacencyList: Map[Int, List[Int]], pathStart: Int, pathEnd: Int): Map[Int, List[Int]] = {
    val neighbours: List[Int] = adjacencyList.getOrElse(pathEnd, Nil)
    adjacencyList.updated(pathEnd, pathStart :: neighbours)
  }

  private def removeExtraEdgeFromCycle(cycle: List[Int], pathStart: Int, pathEnd: Int): List[Int] = {
    cycle.sliding(2).zipWithIndex.find{
      case (List(a, b), _) => a == pathEnd && b == pathStart
      case _ => throw new Exception("Found cycle is too short.")
    }.map(_._2) match {
      case None => cycle
      case Some(ix) => cycle.drop(ix + 1) ::: cycle.slice(1, ix + 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val adjacencyList: Map[Int, List[Int]] = readLines(reader)
    val graph = new EulerianGraph(adjacencyList)
    val result: List[Int] = graph.findEulerianPath()
    println(result.mkString("->"))
  }
}
