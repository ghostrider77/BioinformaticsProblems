package TextbookTrack.Chapter09

object BA9C {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Map => MutableMap}

  final case class Edge(nodeTo: Int, startIx: Int, length: Int)

  class SuffixTree(text: String) {
    private val size: Int = text.length
    private val nodeIdGenerator: Iterator[Int] = Iterator.from(0)
    private val root: Int = nodeIdGenerator.next()
    private val adjacencyList: Map[Int, List[Edge]] = buildSuffixTree()

    def edges(): Iterator[String] =
      adjacencyList
        .valuesIterator
        .flatMap(_.map{ case Edge(_, startIx, length) => text.slice(startIx, startIx + length) })

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
        .map{ case (_, _, jy) => jy}
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next()
    val tree = new SuffixTree(text)
    tree.edges().foreach(println)
  }
}
