package TextbookTrack.Chapter09

object BA9P {
  import scala.annotation.tailrec

  sealed trait NodeColor {
    override def toString: String = this match {
      case Blue => "blue"
      case Red => "red"
      case Purple => "purple"
    }
  }

  case object Blue extends NodeColor
  case object Red extends NodeColor
  case object Purple extends NodeColor

  object NodeColor {
    def apply(s: String): NodeColor = s match {
      case "blue" => Blue
      case "red" => Red
      case "purple" => Purple
      case _ => throw new Exception("Unknown node color.")
    }
  }

  private def readEdges(lines: Iterator[String]): Map[Int, List[Int]] =
    lines
      .takeWhile(_ != "-")
      .map(_.split(" -> ").toList)
      .collect{
        case List(node, neighbours) if neighbours != "{}" => (node.toInt, neighbours.split(",").map(_.toInt).toList)
      }
      .toMap

  private def readLeafColors(lines: Iterator[String]): Map[Int, NodeColor] =
    lines
      .map(_.split(": ").toList)
      .collect{ case List(node, color) => (node.toInt, NodeColor(color)) }
      .toMap

  private def collectRipeNodes(adjacencyList: Map[Int, List[Int]], nodeColors: Map[Int, NodeColor]): List[Int] = {
    (for {
      (node, children) <- adjacencyList
      if !nodeColors.contains(node) && children.forall(nodeColors.contains)
    } yield node).toList
  }

  private def assignColorToNode(node: Int,
                                adjacencyList: Map[Int, List[Int]],
                                nodeColors: Map[Int, NodeColor]): NodeColor = {
    val children: List[Int] = adjacencyList(node)
    val childColors: Set[NodeColor] = children.map(nodeColors).toSet
    if (childColors.size == 1) childColors.head
    else Purple
  }

  def performNodeColoring(adjacencyList: Map[Int, List[Int]], leafColors: Map[Int, NodeColor]): Map[Int, NodeColor] = {
    @tailrec
    def loop(nodeColors: Map[Int, NodeColor]): Map[Int, NodeColor] = collectRipeNodes(adjacencyList, nodeColors) match {
      case Nil => nodeColors
      case ripeNodes =>
        val coloredRipeNodes: List[(Int, NodeColor)] =
          ripeNodes.map{ node => (node, assignColorToNode(node, adjacencyList, nodeColors)) }
        loop(nodeColors ++ coloredRipeNodes)
    }

    if (adjacencyList.isEmpty) leafColors else loop(leafColors)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val adjacencyList: Map[Int, List[Int]] = readEdges(reader)
    val nodeColors: Map[Int, NodeColor] = readLeafColors(reader)
    val result: Map[Int, NodeColor] = performNodeColoring(adjacencyList, nodeColors)
    result.foreach { case (node, color) => println(s"$node: $color") }
  }
}
