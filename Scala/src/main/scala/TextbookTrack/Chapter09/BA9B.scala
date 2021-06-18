package TextbookTrack.Chapter09

object BA9B {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Map => MutableMap}
  import scala.collection.{Map => BothMap}

  private type Node = Int

  private val StringTerminator: Char = '$'

  final case class Edge(nodeFrom: Node, nodeTo: Node, label: Char) {
    override def toString: String = s"$nodeFrom->$nodeTo:$label"
  }

  class Trie(patterns: List[String]) {
    import Trie.getNeighbourWithGivenLabel

    private val nodeIdGenerator: Iterator[Int] = Iterator.from(0)
    private val root: Int = nodeIdGenerator.next()
    private val adjacencyList: Map[Node, List[Edge]] = buildTrieFromPatterns()

    def edges(): Iterator[Edge] = adjacencyList.valuesIterator.flatten

    def isMatching(text: List[Char]): Boolean = {
      @tailrec
      def loop(xs: List[Char], currentNode: Node): Boolean = xs match {
        case Nil => false
        case letter :: xss =>
          getNeighbourWithGivenLabel(adjacencyList, currentNode, letter) match {
            case None => false
            case Some(nextNode) => if (isPatternEnd(nextNode)) true else loop(xss, nextNode)
          }
      }

      loop(text, root)
    }

    private def isPatternEnd(node: Node): Boolean =
      adjacencyList.getOrElse(node, Nil).exists{ case Edge(_, _, label) => label == StringTerminator }

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
    private def getNeighbourWithGivenLabel(adjacencyList: BothMap[Node, List[Edge]],
                                           currentNode: Node,
                                           letter: Char): Option[Node] = {
      adjacencyList
        .get(currentNode)
        .flatMap(_.find{ case Edge(_, _, label) => label == letter })
        .map(_.nodeTo)
    }
  }

  def runTrieMatching(text: String, patterns: List[String]): List[Int] = {
    val trie = new Trie(patterns.map(_ + StringTerminator))
    @tailrec
    def loop(matchingIndices: List[Int], text: List[Char], ix: Int): List[Int] = text match {
      case Nil => matchingIndices.reverse
      case _ :: rest =>
        if (trie.isMatching(text)) loop(ix :: matchingIndices, rest, ix + 1)
        else loop(matchingIndices, rest, ix + 1)
    }

    loop(Nil, text.toList, 0)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val text: String = reader.next()
    val patterns: List[String] = reader.toList
    val result: List[Int] = runTrieMatching(text, patterns)
    println(result.mkString(" "))
  }
}
