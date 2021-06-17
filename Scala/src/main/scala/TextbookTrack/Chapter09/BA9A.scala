package TextbookTrack.Chapter09

object BA9A {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Map => MutableMap}

  private type Node = Int

  final case class Edge(nodeFrom: Node, nodeTo: Node, label: Char) {
    override def toString: String = s"$nodeFrom->$nodeTo:$label"
  }

  class Trie(patterns: List[String]) {
    import Trie.getNeighbourWithGivenLabel

    private val nodeIdGenerator: Iterator[Int] = Iterator.from(0)
    private val root: Int = nodeIdGenerator.next()
    private val adjacencyList: Map[Node, List[Edge]] = buildTrieFromPatterns()

    def edges(): Iterator[Edge] = adjacencyList.valuesIterator.flatten

    private def buildTrieFromPatterns(): Map[Node, List[Edge]] = {
      val adjacencyList: MutableMap[Node, List[Edge]] = MutableMap()
      patterns.foreach(addPatternToTrie(adjacencyList, _))
      adjacencyList.toMap
    }

    private def addPatternToTrie(adjacencyList: MutableMap[Node, List[Edge]], pattern: String): Unit = {
      @tailrec
      def loop(letters: List[Char], currentNode: Node): Unit = letters match {
        case Nil => ()
        case letter :: lss =>
          getNeighbourWithGivenLabel(adjacencyList, currentNode, letter) match {
            case None =>
              val nextNode: Node = nodeIdGenerator.next()
              val neighbours: List[Edge] = adjacencyList.getOrElse(currentNode, Nil)
              adjacencyList.update(currentNode, Edge(currentNode, nextNode, letter) :: neighbours)
              loop(lss, nextNode)
            case Some(node) => loop(lss, node)
        }
      }

      loop(pattern.toList, root)
    }
  }

  object Trie {
    private def getNeighbourWithGivenLabel(adjacencyList: MutableMap[Node, List[Edge]],
                                           currentNode: Node,
                                           letter: Char): Option[Node] = {
      adjacencyList
        .get(currentNode)
        .flatMap(_.find{ case Edge(_, _, label) => label == letter })
        .map(_.nodeTo)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val patterns: List[String] = reader.toList
    val trie = new Trie(patterns)
    trie.edges().foreach(println)
  }
}
