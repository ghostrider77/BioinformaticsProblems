package TextbookTrack.Chapter07

object BA7A {
  final case class WeightedEdge(nodeFrom: Int, nodeTo: Int, weight: Int)

  class Tree(val adjacencyList: Map[Int, List[WeightedEdge]], val nrLeaves: Int) {
    lazy val nrNodes: Int = adjacencyList.valuesIterator.flatMap(_.map(_.nodeTo)).max + 1

    def calcPairwiseDistances(): List[List[Int]] = {
      val distances: Array[Array[Double]] = initializeDistanceMatrix()
      for {
        k <- 0 until nrNodes
        ix <- 0 until nrNodes
        jy <- 0 until nrNodes
      } distances(ix)(jy) = math.min(distances(ix)(jy), distances(ix)(k) + distances(k)(jy))

      distances.map(row => row.map(_.toInt).toList).toList
    }

    private def initializeDistanceMatrix(): Array[Array[Double]] = {
      val distances: Array[Array[Double]] = Array.fill(nrNodes, nrNodes)(Double.PositiveInfinity)
      for {
        (node, neighbours) <- adjacencyList
        WeightedEdge(_, neighbour, weight) <- neighbours
      } distances(node)(neighbour) = weight

      (0 until nrNodes).foreach(node => distances(node)(node) = 0)

      distances
    }
  }

  private def splitEdge(line: String): WeightedEdge = line.replace("->", ":").split(":").map(_.toInt).toList match {
    case List(from, to, weight) => WeightedEdge(from, to, weight)
    case _ => throw new Exception("Malformed input data.")
  }

  def readWeightedEdges(lines: Iterator[String]): Map[Int, List[WeightedEdge]] =
    lines.map(splitEdge).toList.groupBy(_.nodeFrom)

  def computeDistancesBetweenLeaves(tree: Tree): List[List[Int]] = {
    val distances: List[List[Int]] = tree.calcPairwiseDistances()
    distances.take(tree.nrLeaves).map(_.take(tree.nrLeaves))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrLeaves: Int = reader.next().toInt
    val adjacencyList: Map[Int, List[WeightedEdge]] = readWeightedEdges(reader)
    val tree = new Tree(adjacencyList, nrLeaves)
    val result: List[List[Int]] = computeDistancesBetweenLeaves(tree)
    result.foreach(line => println(line.mkString(" ")))
  }
}
