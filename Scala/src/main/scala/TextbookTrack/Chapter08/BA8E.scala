package TextbookTrack.Chapter08

object BA8E {
  import scala.annotation.tailrec

  final case class Cluster(id: Int, content: Vector[Int]) {
    val size: Int = content.length
  }

  final case class IndexPair(i: Int, j: Int) {
    require(i <= j, s"Index $i should be less than index $j.")

    def contains(k: Int): Boolean = (k == i) || (k == j)
  }

  class DistanceMatrix(distances: Map[IndexPair, Double]) {
    def apply(key: IndexPair): Double = distances(key)

    private def clusterIds: Set[Int] = distances.keysIterator.flatMap{ case IndexPair(i, j) => Set(i, j) }.toSet

    def closestClusters(): IndexPair = distances.minBy{ case (_, distance) => distance }._1

    def update(cluster1: Cluster, cluster2: Cluster, nextId: Int): DistanceMatrix = {
      val updatedDistances: Map[IndexPair, Double] =
        clusterIds.map{
          id =>
            val pair1 = IndexPair(math.min(id, cluster1.id), math.max(id, cluster1.id))
            val pair2 = IndexPair(math.min(id, cluster2.id), math.max(id, cluster2.id))
            val num: Double =
              distances.getOrElse(pair1, 0.0) * cluster1.size + distances.getOrElse(pair2, 0.0) * cluster2.size
            IndexPair(id, nextId) -> num / (cluster1.size + cluster2.size)
        }.toMap ++ distances

      val reducedDistances: Map[IndexPair, Double] =
        updatedDistances.filter{ case (key, _) => !key.contains(cluster1.id) && !key.contains(cluster2.id)}
      new DistanceMatrix(reducedDistances)
    }
  }

  private def convertToDoubleList(line: String): List[Double] = line.split(" ").map(_.toDouble).toList

  def readDistanceMatrix(reader: Iterator[String], n: Int): DistanceMatrix = {
    val adjacencyList: Map[IndexPair, Double] =
      reader
        .map(convertToDoubleList)
        .take(n)
        .zipWithIndex
        .flatMap{ case (row, ix) => row.zipWithIndex.drop(ix + 1).map{ case (dist, jy) => (IndexPair(ix, jy), dist) } }
        .toMap
    new DistanceMatrix(adjacencyList)
  }

  def runHierarchicalClustering(distances: DistanceMatrix, n: Int): List[Cluster] = {
    @tailrec
    def loop(clusters: Map[Int, Cluster],
             distances: DistanceMatrix,
             newClusters: List[Cluster],
             nextId: Int): List[Cluster] = {
      if (clusters.size <= 1) newClusters.reverse
      else {
        val IndexPair(id1, id2): IndexPair = distances.closestClusters()
        val cluster1: Cluster = clusters(id1)
        val cluster2: Cluster = clusters(id2)
        val nextCluster = Cluster(id = nextId, content = cluster1.content ++ cluster2.content)
        val updatedClusters: Map[Int, Cluster] = clusters -- Set(id1, id2) + (nextId -> nextCluster)
        loop(updatedClusters, distances.update(cluster1, cluster2, nextId), nextCluster :: newClusters, nextId + 1)
      }
    }

    val initialClusters: Map[Int, Cluster] = (0 until n).map(k => k -> Cluster(k, Vector(k))).toMap
    loop(initialClusters, distances, Nil, n)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val distances: DistanceMatrix = readDistanceMatrix(reader, n)
    val result: List[Cluster] = runHierarchicalClustering(distances, n)
    result.foreach{ case Cluster(_, content) => println(content.map(_ + 1).mkString(" ")) }
  }
}
