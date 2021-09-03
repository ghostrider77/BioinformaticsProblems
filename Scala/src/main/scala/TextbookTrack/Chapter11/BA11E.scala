package TextbookTrack.Chapter11

object BA11E {
  import scala.annotation.tailrec
  import TextbookTrack.Utils.IntegerMassTable

  final case class Node(id: Int, weight: Int)
  final case class Edge(nodeFrom: Node, nodeTo: Node, label: Char)

  class SpectralGraph(spectrum: List[Int], inverseMassTable: Map[Int, Char]) {
    import SpectralGraph.{buildSpectrumGraph, calcIncomingEdges, createNodes}

    val nodes: List[Node] = createNodes(spectrum)
    val adjacencyList: Map[Node, List[Edge]] = buildSpectrumGraph(nodes, inverseMassTable)
    private val incomingEdges: Map[Node, List[Node]] = calcIncomingEdges(adjacencyList)

    val source: Node = nodes.head
    val sink: Node = nodes.last

    def predecessors(node: Node): List[Node] = incomingEdges.getOrElse(node, Nil)
  }

  object SpectralGraph {
    private def calcIncomingEdges(adjacencyList: Map[Node, List[Edge]]): Map[Node, List[Node]] = {
      val backwardEdges: List[(Node, Node)] =
        (for {
          (node, neighbours) <- adjacencyList.iterator
          Edge(_, neighbour, _) <- neighbours
        } yield (neighbour, node)).toList
      backwardEdges.groupMap{ case (neighbour, _) => neighbour }{ case (_, node) => node }
    }

    private def createNodes(spectrum: List[Int]): List[Node] =
      Node(0, 0) :: spectrum.zipWithIndex.map{ case (s, ix) => Node(id = ix + 1, weight = s)}

    private def buildSpectrumGraph(nodes: List[Node], inverseMassTable: Map[Int, Char]): Map[Node, List[Edge]] = {
      val edges: List[Edge] = for {
        (node1, ix) <- nodes.zipWithIndex
        node2 <- nodes.drop(ix)
        mass: Int = node2.id - node1.id
        label <- inverseMassTable.get(mass)
      } yield Edge(node1, node2, label)
      edges.groupBy(_.nodeFrom)
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def reverseMassTableMapping(massTable: Map[Char, Int]): Map[Int, Char] = massTable.map(_.swap)

  private def findLongestPath(graph: SpectralGraph): List[Node] = {
    @tailrec
    def loop(longestPath: Map[Node, Double],
             backtrack: Map[Node, Node],
             nodes: List[Node]): Map[Node, Node] = nodes match {
      case Nil => backtrack
      case node :: rest =>
        graph.predecessors(node) match {
          case Nil => loop(longestPath, backtrack, rest)
          case predecessors =>
            val paths: List[(Node, Double)] =
              predecessors.map(u => (u, longestPath.getOrElse(u, Double.NegativeInfinity) + node.weight))
            val (v, s): (Node, Double) = paths.maxBy{ case (_, cost) => cost }
            loop(longestPath + (node -> s), backtrack + (node -> v), rest)
        }
    }

    val backtrack: Map[Node, Node] = loop(Map(graph.source -> 0.0), Map(), graph.nodes)
    collectLongestPath(backtrack, graph.source, graph.sink)
  }

  private def collectLongestPath(backtrack: Map[Node, Node], source: Node, sink: Node): List[Node] = {
    @tailrec
    def loop(acc: List[Node], currentNode: Node): List[Node] = {
      if (currentNode == source) acc
      else {
        val nextNode: Node = backtrack(currentNode)
        loop(nextNode :: acc, nextNode)
      }
    }
    loop(List(sink), sink)
  }

  private def restorePeptideFromPath(path: List[Node], inverseMassTable: Map[Int, Char]): String =
    path
      .sliding(2)
      .collect{ case List(Node(nodeId1, _), Node(nodeId2, _)) => inverseMassTable(nodeId2 - nodeId1) }
      .mkString

  def runPeptideSequencing(spectrum: List[Int], massTable: Map[Char, Int]): String = {
    val inverseMassTable: Map[Int, Char] = reverseMassTableMapping(massTable)
    val graph = new SpectralGraph(spectrum, inverseMassTable)
    val path: List[Node] = findLongestPath(graph)
    restorePeptideFromPath(path, inverseMassTable)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val spectrum: List[Int] = convertToIntList(reader.next())
    val result: String = runPeptideSequencing(spectrum, IntegerMassTable)
    println(result)
  }
}
