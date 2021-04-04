package TextbookTrack.Chapter07

object BA7D {
  import scala.annotation.tailrec

  final case class Node(id: Int, age: Double, size: Int)

  final case class WeightedEdge(nodeFrom: Int, nodeTo: Int, weight: Double) {
    override def toString: String = s"$nodeFrom->$nodeTo:${math.round(1000 * weight) / 1000.toDouble}"
  }

  final case class IndexPair(i: Int, j: Int) {
    require(i <= j, s"Index $i should be less than index $j.")

    def contains(k: Int): Boolean = (k == i) || (k == j)
  }

  class DistanceMatrix(distances: Map[IndexPair, Double]) {
    def apply(key: IndexPair): Double = distances(key)

    private def clusterIds: Set[Int] = distances.keysIterator.flatMap{ case IndexPair(i, j) => Set(i, j) }.toSet

    def closestClusters(): IndexPair = distances.minBy{ case (_, distance) => distance }._1

    def update(node1: Node, node2: Node, nextId: Int): DistanceMatrix = {
      val updatedDistances: Map[IndexPair, Double] =
        clusterIds.map{
          id =>
            val pair1 = IndexPair(math.min(id, node1.id), math.max(id, node1.id))
            val pair2 = IndexPair(math.min(id, node2.id), math.max(id, node2.id))
            val num: Double =
              distances.getOrElse(pair1, 0.0) * node1.size + distances.getOrElse(pair2, 0.0) * node2.size
            IndexPair(id, nextId) -> num / (node1.size + node2.size)
        }.toMap ++ distances

      val reducedDistances: Map[IndexPair, Double] =
        updatedDistances.filter{ case (key, _) => !key.contains(node1.id) && !key.contains(node2.id)}
      new DistanceMatrix(reducedDistances)
    }
  }

  private def convertToDoubleList(line: String): List[Double] = line.split(" ").map(_.toDouble).toList

  def readDistanceMatrix(reader: Iterator[String], nrLeaves: Int): DistanceMatrix = {
    val adjacencyList: Map[IndexPair, Double] = {
      reader
      .map(convertToDoubleList)
      .take(nrLeaves)
      .zipWithIndex
      .flatMap{
        case (row, ix) => row.zipWithIndex.drop(ix + 1).map{ case (dist, jy) => (IndexPair(ix, jy), dist) }
      }.toMap
    }
    new DistanceMatrix(adjacencyList)
  }

  private def getTreeEdges(tree: Map[Node, (Node, Node)]): List[WeightedEdge] =
    tree
      .flatMap{
        case (Node(nodeId, nodeAge, _), (child1, child2)) =>
          List(child1, child2).flatMap{ case Node(childId, childAge, _) =>
            val weight: Double = nodeAge - childAge
            List(WeightedEdge(nodeId, childId, weight), WeightedEdge(childId, nodeId, weight))
          }
      }
      .toList
      .sortBy(_.nodeFrom)

  def performUpgmaClustering(distances: DistanceMatrix, nrLeaves: Int): List[WeightedEdge] = {
    @tailrec
    def loop(clusters: Map[Int, Node],
             distances: DistanceMatrix,
             adjacencyList: Map[Node, (Node, Node)],
             nextId: Int): Map[Node, (Node, Node)] = {
      if (clusters.size <= 1) adjacencyList
      else {
        val pair @ IndexPair(id1, id2): IndexPair = distances.closestClusters()
        val dist: Double = distances(pair)
        val node1: Node = clusters(id1)
        val node2: Node = clusters(id2)
        val nextNode = Node(id = nextId, age = dist/2, size = node1.size + node2.size)
        val updatedClusters: Map[Int, Node] = clusters -- Set(id1, id2) + (nextId -> nextNode)
        val updatedAdjacencyList: Map[Node, (Node, Node)] = adjacencyList + (nextNode -> (node1, node2))
        loop(updatedClusters, distances.update(node1, node2, nextId), updatedAdjacencyList, nextId + 1)
      }
    }

    val initialClusters: Map[Int, Node] = (0 until nrLeaves).map(k => k -> Node(k, 0.0, 1)).toMap
    val tree: Map[Node, (Node, Node)] = loop(initialClusters, distances, Map(), nrLeaves)
    getTreeEdges(tree)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrLeaves: Int = reader.next().toInt
    val distances: DistanceMatrix = readDistanceMatrix(reader, nrLeaves)
    val result: List[WeightedEdge] = performUpgmaClustering(distances, nrLeaves)
    result.foreach(println)
  }
}
