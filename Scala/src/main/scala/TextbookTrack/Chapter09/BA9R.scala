package TextbookTrack.Chapter09

object BA9R {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Map => MutableMap}

  final case class Node(id: Int, parentId: Int, depth: Int)
  final case class Edge(nodeTo: Int, startIx: Int, length: Int)

  class SuffixTree(text: String, suffixArray: List[Int], lcpArray: List[Int]) {
    private val size: Int = text.length
    private val nodeIdGenerator: Iterator[Int] = Iterator.from(0)
    private val adjacencyList: Map[Int, List[Edge]] = buildSuffixTree()

    def edges(): Iterator[String] =
      adjacencyList
        .valuesIterator
        .flatMap(_.map{ case Edge(_, startIx, length) => text.slice(startIx, startIx + length) })

    private def buildSuffixTree(): Map[Int, List[Edge]] = {
      val adjacencyList: MutableMap[Int, List[Edge]] = MutableMap()
      val nodes: MutableMap[Int, Node] = MutableMap()

      @tailrec
      def findStartingNodeForNextEdge(node: Node, lcp: Int): Node = {
        if (node.depth > lcp) findStartingNodeForNextEdge(nodes(node.parentId), lcp)
        else node
      }

      @tailrec
      def loop(xs: List[(Int, Int)], currentNode: Node): Map[Int, List[Edge]] = xs match {
        case Nil => adjacencyList.toMap
        case (suffixStart, lcp) :: xss =>
          val node: Node = findStartingNodeForNextEdge(currentNode, lcp)
          val substringStart: Int = suffixStart + node.depth
          if (node.depth == lcp) loop(xss, addLeaf(adjacencyList, node, nodes, substringStart))
          else {
            val edge: Edge = identifyEdge(adjacencyList, node, substringStart)
            val offset: Int = lcp - node.depth
            val middleNode: Node = splitEdge(node, edge, offset, nodes, adjacencyList)
            loop(xss, addLeaf(adjacencyList, middleNode, nodes, substringStart + offset))
          }
      }

      val rootId: Int = nodeIdGenerator.next()
      val root: Node = Node(rootId, -1, 0)
      nodes(rootId) = root
      loop(suffixArray.zip(lcpArray), root)
    }

    private def addLeaf(adjacencyList: MutableMap[Int, List[Edge]],
                        node: Node,
                        nodes: MutableMap[Int, Node],
                        substringStart: Int): Node = {
      val leafId: Int = nodeIdGenerator.next()
      val labelLength: Int = size - substringStart
      val leaf: Node = Node(id = leafId, parentId = node.id, depth = node.depth + labelLength)
      nodes(leafId) = leaf
      adjacencyList.update(node.id, Edge(leafId, substringStart, labelLength) :: adjacencyList.getOrElse(node.id, Nil))
      leaf
    }

    private def identifyEdge(adjacencyList: MutableMap[Int, List[Edge]], node: Node, substringStart: Int): Edge = {
      val char: Char = text(substringStart)
      adjacencyList(node.id).find{ case Edge(_, startIx, _) => text(startIx) == char } match {
        case Some(edge) => edge
        case None => throw new Exception("Edge not found.")
      }
    }

    private def splitEdge(node: Node,
                          edge: Edge,
                          offset: Int,
                          nodes: MutableMap[Int, Node],
                          adjacencyList: MutableMap[Int, List[Edge]]): Node = {
      val endNode: Node = nodes(edge.nodeTo)
      val middleNodeId: Int = nodeIdGenerator.next()
      val middleNode: Node = Node(id = middleNodeId, parentId = node.id, depth = node.depth + offset)
      nodes(middleNodeId) = middleNode
      nodes(endNode.id) = endNode.copy(parentId = middleNodeId)
      val firstEdge: Edge = Edge(nodeTo = middleNodeId, startIx = edge.startIx, length = offset)
      val secondEdge: Edge = Edge(nodeTo = edge.nodeTo, startIx = edge.startIx + offset, length = edge.length - offset)
      adjacencyList.update(middleNodeId, secondEdge :: adjacencyList.getOrElse(middleNodeId, Nil))
      adjacencyList.update(node.id, firstEdge :: adjacencyList.getOrElse(node.id, Nil))
      adjacencyList.update(node.id, adjacencyList(node.id).filterNot(_ == edge))
      middleNode
    }
  }

  private def convertToIntlist(line: String, sep: String = ", "): List[Int] =
    line.split(sep).map(_.toInt).toList

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next()
    val suffixArray: List[Int] = convertToIntlist(reader.next())
    val lcpArray: List[Int] = convertToIntlist(reader.next())
    val suffixTree = new SuffixTree(text, suffixArray, lcpArray)
    suffixTree.edges().foreach(println)
  }
}
