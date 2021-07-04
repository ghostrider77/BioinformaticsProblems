package TextbookTrack.Chapter09

object BA9E {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Map => MutableMap}

  sealed trait NodeColor
  final case object Blue extends NodeColor
  final case object Red extends NodeColor
  final case object Purple extends NodeColor
  final case object Gray extends NodeColor

  final case class Edge(nodeTo: Int, startIx: Int, length: Int)

  class SuffixTree(text1: String, text2: String) {
    val text: String = text1 + text2

    private val text1Size: Int = text1.length
    private val size: Int = text.length
    private val nodeIdGenerator: Iterator[Int] = Iterator.from(0)

    val root: Int = nodeIdGenerator.next()
    val adjacencyList: Map[Int, List[Edge]] = buildSuffixTree()

    lazy val nrNodes: Int =
      adjacencyList.valuesIterator.flatten.foldLeft(0){ case (n, Edge(nodeTo, _, _)) => math.max(n, nodeTo) } + 1

    def doesEdgeBelongToFirstText(edge: Edge): Boolean = edge.startIx < text1Size

    private def buildSuffixTree(): Map[Int, List[Edge]] = {
      val adjacencyList: MutableMap[Int, List[Edge]] = MutableMap()
      (0 until size).foreach(addSuffixToTree(adjacencyList, _))
      adjacencyList.toMap
    }

    private def addSuffixToTree(adjacencyList: MutableMap[Int, List[Edge]], suffixStart: Int): Unit = {
      val (node, optionalEdge, edgeStartIx, nonMatchingIx): (Int, Option[Edge], Int, Int) =
        findLastMatchingNode(adjacencyList, suffixStart)
      val newLeaf: Int = nodeIdGenerator.next()
      optionalEdge match {
        case None =>
          adjacencyList
            .update(node, Edge(newLeaf, edgeStartIx, size - edgeStartIx) :: adjacencyList.getOrElse(node, Nil))
        case Some(edge @ Edge(nodeTo, startIx, length)) =>
          val newNode: Int = nodeIdGenerator.next()
          val splitLength1: Int = nonMatchingIx - edgeStartIx
          val splitLength2: Int = length - splitLength1
          adjacencyList(node) = adjacencyList(node).filterNot(_ == edge)
          adjacencyList.update(node, Edge(newNode, startIx, splitLength1) :: adjacencyList.getOrElse(node, Nil))
          adjacencyList(newNode) =
            List(Edge(nodeTo, startIx + splitLength1, splitLength2), Edge(newLeaf, nonMatchingIx, size - nonMatchingIx))
      }
    }

    private def findLastMatchingNode(adjacencyList: MutableMap[Int, List[Edge]],
                                     suffixStart: Int): (Int, Option[Edge], Int, Int) = {
      def findNextEdgeInPath(node: Int, letter: Char): Option[Edge] =
        adjacencyList.getOrElse(node, Nil).find{ case Edge(_, startIx, _) => text(startIx) == letter }

      @tailrec
      def loop(currentNode: Int, ix: Int): (Int, Option[Edge], Int, Int) =
        findNextEdgeInPath(currentNode, text(ix)) match {
          case None => (currentNode, None, ix, ix)
          case Some(edge @ Edge(nodeTo, _, length)) =>
            findFirstNonMatchingIndex(ix, edge) match {
              case Some(nonMatchingIx) => (currentNode, Some(edge), ix, nonMatchingIx)
              case None => loop(nodeTo, ix + length)
            }
        }

      loop(root, suffixStart)
    }

    private def findFirstNonMatchingIndex(ix: Int, edge: Edge): Option[Int] = {
      val Edge(_, startIx, length) = edge
      val substring: String = text.slice(startIx, startIx + length)
      substring
        .lazyZip(text.substring(ix))
        .lazyZip(ix until size)
        .find{ case (a, b, _) => a != b }
        .map{ case (_, _, jy) => jy }
    }
  }

  private def performNodeColoring(suffixTree: SuffixTree): Vector[NodeColor] = {
    val nodeColors: Array[NodeColor] = initializeNodeColors(suffixTree)

    @tailrec
    def loop(ripeNodes: List[Int]): Unit = {
      if (ripeNodes.nonEmpty) {
        ripeNodes.foreach{
          node =>
            val children: Set[Edge] = suffixTree.adjacencyList.getOrElse(node, Nil).toSet
            val colors: Set[NodeColor] = children.map{ case Edge(nodeTo, _, _) => nodeColors(nodeTo) }
            nodeColors(node) = if (colors.size == 1) colors.iterator.next() else Purple
        }
        loop(findRipeNodes(suffixTree, nodeColors))
      }
    }

    loop(findRipeNodes(suffixTree, nodeColors))
    nodeColors.toVector
  }

  private def initializeNodeColors(suffixTree: SuffixTree): Array[NodeColor] = {
    val nodeColors: Array[NodeColor] = Array.fill(suffixTree.nrNodes)(Gray)

    @tailrec
    def loop(currentNodes: List[(Int, NodeColor)]): Unit = {
      if (currentNodes.nonEmpty) {
        val (leaves, innerNodes): (List[((Int, NodeColor), List[Edge])], List[((Int, NodeColor), List[Edge])]) =
          currentNodes
            .map{ case pair @ (node, _) => (pair, suffixTree.adjacencyList.getOrElse(node, Nil)) }
            .partition{ case (_, edges) => edges.isEmpty }

        leaves.foreach{ case ((node, nodeColor), _) => nodeColors(node) = nodeColor }
        val nextNodes: List[(Int, NodeColor)] =
          innerNodes.flatMap{ case (_, edges) => collectEdgeEndpoints(suffixTree, edges) }
        loop(nextNodes)
      }
    }

    loop(List((suffixTree.root, Gray)))
    nodeColors
  }

  private def collectEdgeEndpoints(suffixTree: SuffixTree, edges: List[Edge]): List[(Int, NodeColor)] =
    edges.map{ case edge @ Edge(nodeTo, _, _) =>
      if (suffixTree.doesEdgeBelongToFirstText(edge)) (nodeTo, Blue) else (nodeTo, Red)
    }

  private def findRipeNodes(suffixTree: SuffixTree, nodeColors: Array[NodeColor]): List[Int] =
    nodeColors
      .iterator
      .zipWithIndex
      .filter{ case (color, _) => color == Gray }
      .filter{ case (_, node) =>
        val children = suffixTree.adjacencyList.getOrElse(node, Nil)
        children.forall{ case Edge(nodeTo, _, _) => nodeColors(nodeTo) != Gray }
      }
      .map{ case (_, node) => node }
      .toList

  private def findLongestSubstringSpelledByPurpleNodes(suffixTree: SuffixTree,
                                                       nodeColors: Vector[NodeColor]): String = {
      @tailrec
      def loop(longestSharedSubstring: String, currentNodes: List[(Int, String)]): String = {
        if (currentNodes.isEmpty) longestSharedSubstring
        else {
          val nextNodes: List[(Int, String)] =
            currentNodes.flatMap{
              case (node, spelledString) =>
                getPurpleNeighbourNodes(suffixTree, nodeColors, node, spelledString)
            }
          loop(updateLongestSharedSubstring(longestSharedSubstring, nextNodes), nextNodes)
        }
      }

      val emptyString: String = ""
      loop(emptyString, List((suffixTree.root, emptyString)))
    }

  private def getPurpleNeighbourNodes(suffixTree: SuffixTree,
                                      nodeColors: Vector[NodeColor],
                                      node: Int,
                                      spelledString: String): List[(Int, String)] = {
    suffixTree
      .adjacencyList
      .getOrElse(node, Nil)
      .withFilter{ case Edge(nodeTo, _, _) => nodeColors(nodeTo) == Purple }
      .map{ case Edge(nodeTo, startIx, length) =>
        (nodeTo, spelledString + suffixTree.text.slice(startIx, startIx + length))
      }
  }

  private def updateLongestSharedSubstring(substring: String, nodes: List[(Int, String)]): String =
    nodes.foldLeft(substring){ case (acc, (_, string)) => if (string.length > acc.length) string else acc }

  def findLongestSharedSubstring(text1: String, text2: String): String = {
    val suffixTree = new SuffixTree(text1 + '#', text2 + '$')
    val nodeColors: Vector[NodeColor] = performNodeColoring(suffixTree)
    findLongestSubstringSpelledByPurpleNodes(suffixTree, nodeColors)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text1: String = reader.next()
    val text2: String = reader.next()
    val result: String = findLongestSharedSubstring(text1, text2)
    println(result)
  }
}
