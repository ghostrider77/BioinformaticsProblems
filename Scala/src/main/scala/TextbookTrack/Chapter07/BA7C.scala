package TextbookTrack.Chapter07

object BA7C {
  import scala.annotation.tailrec
  import scala.collection.mutable.{Queue => MutableQueue, Map => MutableMap}

  private type DistanceMatrix = Vector[Vector[Int]]

  final case class WeightedEdge(nodeFrom: Int, nodeTo: Int, weight: Int)
  final case class AttachmentPoint(startLeaf: Int, endLeaf: Int, distanceFromStart: Int)

  class Tree(val adjacencyList: Map[Int, List[WeightedEdge]], val nrLeaves: Int) {
    import Tree.resolvePathBetweenLeaves

    private def largestLeaf: Int = adjacencyList.keysIterator.filter(_ < nrLeaves).max

    private def largestInnerNode: Int = {
      val innerNodes: Iterator[Int] = adjacencyList.keysIterator.filter(_ >= nrLeaves)
      if (innerNodes.isEmpty) nrLeaves - 1
      else innerNodes.max
    }

    def edges: Iterator[String] = {
      val orderedNodes: List[Int] = adjacencyList.keysIterator.toList.sorted
      for {
        node <- orderedNodes.iterator
        neighbours: List[WeightedEdge] = adjacencyList(node)
        WeightedEdge(_, neighbour, weight) <- neighbours
      } yield s"$node->$neighbour:$weight"
    }

    def insertLeafIntoTree(limblength: Int, leafStart: Int, leafEnd: Int, insertionDistance: Int): Tree = {
      @tailrec
      def loop(path: List[WeightedEdge], pathLength: Int): Tree = path match {
        case Nil => throw new Exception("Insertion distance should be less than the total length of the path.")
        case WeightedEdge(currentNode, nextNode, weight) :: rest =>
          val currrentPathLength: Int = pathLength + weight
          if (insertionDistance < currrentPathLength) {
            val distanceFromCurrentNode: Int = weight - (currrentPathLength - insertionDistance)
            insertLeaf(currentNode, nextNode, weight, distanceFromCurrentNode, limblength)
          } else loop(rest, currrentPathLength)
      }

      val path: List[WeightedEdge] = breadthFirstSearch(leafStart, leafEnd)
      loop(path, 0)
    }

    private def breadthFirstSearch(startNode: Int, endNode: Int): List[WeightedEdge] = {
      val distances: MutableMap[Int, Int] = MutableMap(startNode -> 0)
      val backtrack: MutableMap[Int, WeightedEdge] = MutableMap()
      val queue: MutableQueue[Int] = MutableQueue(startNode)
      while (queue.nonEmpty) {
        val node: Int = queue.dequeue()
        val neighbours: List[WeightedEdge] = adjacencyList.getOrElse(node, Nil)
        neighbours.foreach{
          case edge @ WeightedEdge(_, neighbour, weight) =>
            if (!distances.contains(neighbour)) {
              queue.enqueue(neighbour)
              distances(neighbour) = distances(node) + weight
              backtrack(neighbour) = edge
            }
        }
      }
      resolvePathBetweenLeaves(backtrack.toMap, startNode, endNode)
    }

    private def insertLeaf(currentNode: Int,
                           nextNode: Int,
                           weight: Int,
                           distanceFromCurrentNode: Int,
                           limblength: Int): Tree = {
      val nextLeaf: Int = largestLeaf + 1
      val updatedAdjacencyList: Map[Int, List[WeightedEdge]] = {
        if (distanceFromCurrentNode == 0) {
          val updatedNeighbours: List[WeightedEdge] =
            WeightedEdge(currentNode, nextLeaf, limblength) :: adjacencyList(currentNode)
          adjacencyList.updated(currentNode, updatedNeighbours) +
            (nextLeaf -> List(WeightedEdge(nextLeaf, currentNode, limblength)))
        }
        else {
          val middleNode: Int = largestInnerNode + 1
          val insertedMiddleNode1: Map[Int, List[WeightedEdge]] =
            addMiddleNode(adjacencyList, currentNode, nextNode, middleNode, distanceFromCurrentNode)
          val insertedMiddleNode2: Map[Int, List[WeightedEdge]] =
            addMiddleNode(insertedMiddleNode1, nextNode, currentNode, middleNode, weight - distanceFromCurrentNode)
          Map(
            middleNode -> List(
              WeightedEdge(middleNode, currentNode, distanceFromCurrentNode),
              WeightedEdge(middleNode, nextNode, weight - distanceFromCurrentNode),
              WeightedEdge(middleNode, nextLeaf, limblength)),
            nextLeaf -> List(WeightedEdge(nextLeaf, middleNode, limblength))
          ) ++ insertedMiddleNode2
        }
      }

      new Tree(updatedAdjacencyList, nrLeaves)
    }

    private def addMiddleNode(adjacencyList: Map[Int, List[WeightedEdge]],
                              currentNode: Int,
                              neighbour: Int,
                              middleNode: Int,
                              distance: Int): Map[Int, List[WeightedEdge]] = {
      val newNeighbours: List[WeightedEdge] =
        WeightedEdge(currentNode, middleNode, distance) :: adjacencyList(currentNode).filter(_.nodeTo != neighbour)
      adjacencyList.updated(currentNode, newNeighbours)
    }
  }

  object Tree {
    private def resolvePathBetweenLeaves(backtrack: Map[Int, WeightedEdge],
                                         startNode: Int,
                                         endNode: Int): List[WeightedEdge] = {
      @tailrec
      def loop(path: List[WeightedEdge], currentNode: Int): List[WeightedEdge] = {
        if (currentNode == startNode) path
        else {
          val edge @ WeightedEdge(previousNode, _, _) = backtrack(currentNode)
          loop(edge :: path, previousNode)
        }
      }
      loop(Nil, endNode)
    }
  }

  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  private def readDistanceMatrix(reader: Iterator[String], nrLeaves: Int): DistanceMatrix =
    reader.take(nrLeaves).map(convertToIntVector).toVector

  private def calcLimbLength(distances: DistanceMatrix, leafJ: Int, nrLeaves: Int): Int = {
    val otherLeaves: List[Int] = (0 until nrLeaves).filterNot(_ == leafJ).toList
    otherLeaves
      .iterator
      .flatMap(leafI => otherLeaves.map(leafK => (leafI, leafK)))
      .foldLeft(Int.MaxValue) {
        case (limbLength, (leafI, leafK)) =>
          val candidate: Int = (distances(leafI)(leafJ) + distances(leafJ)(leafK) - distances(leafI)(leafK)) / 2
          math.min(candidate, limbLength)
      }
  }

  private def subtractLimbLength(distances: DistanceMatrix, leaf: Int, limblength: Int): DistanceMatrix =
    distances.zipWithIndex.map {
      case (row, ix) =>
        if (ix == leaf) {
          row.zipWithIndex.map { case (item, jy) => if (jy == leaf) 0 else item - limblength }
        } else row.zipWithIndex.map{ case (item, jy) => if (jy == leaf) item - limblength else item }
    }

  private def findPathWhereLeafShouldBeAttached(distances: DistanceMatrix, leaf: Int): (Int, Int) = {
    val size: Int = distances.length
    (0 until size)
      .iterator
      .flatMap(leafI => (0 until size).map(leafK => (leafI, leafK)))
      .find{
        case (leafI, leafK) =>
          distances(leafI)(leafK) == distances(leafI)(leaf) + distances(leaf)(leafK)
      } match {
      case None => throw new Exception("No path endpoints found where the leaf should be attached.")
      case Some(leafPair) => leafPair
    }
  }

  private def removeLastRowAndColumn(baldDistances: DistanceMatrix): DistanceMatrix =
    baldDistances.map(_.dropRight(1)).dropRight(1)

  private def createInitialTree(distance: Int, nrLeaves: Int): Tree = {
    val adjacencyList: Map[Int, List[WeightedEdge]] =
      Map(0 -> List(WeightedEdge(0, 1, distance)), 1 -> List(WeightedEdge(1, 0, distance)))
    new Tree(adjacencyList, nrLeaves)
  }

  private def iterativelyReduceDistanceMatrix(distances: DistanceMatrix,
                                              nrLeaves: Int): (Tree, Map[Int, Int], Map[Int, AttachmentPoint]) = {
    @tailrec
    def loop(limblengths: Map[Int, Int],
             attachmentPoints: Map[Int, AttachmentPoint],
             distances: DistanceMatrix,
             leaf: Int): (Tree, Map[Int, Int], Map[Int, AttachmentPoint]) = {
      if (leaf <= 1) {
        val leafDistance: Int = distances(0)(1)
        val tree: Tree = createInitialTree(leafDistance, nrLeaves)
        (tree, limblengths, attachmentPoints)
      } else {
        val limblength: Int = calcLimbLength(distances, leaf, leaf + 1)
        val baldDistances: DistanceMatrix = subtractLimbLength(distances, leaf, limblength)
        val (leafI, leafK): (Int, Int) = findPathWhereLeafShouldBeAttached(baldDistances, leaf)
        val dist: Int = baldDistances(leafI)(leaf)
        val updatedLimbLengths: Map[Int, Int] = limblengths + (leaf -> limblength)
        val updatedAttachmentPoints: Map[Int, AttachmentPoint] =
          attachmentPoints + (leaf -> AttachmentPoint(leafI, leafK, dist))
        loop(updatedLimbLengths, updatedAttachmentPoints, removeLastRowAndColumn(baldDistances), leaf - 1)
      }
    }

    loop(Map(), Map(), distances, nrLeaves - 1)
  }

  def additivePhylogeny(distances: DistanceMatrix, nrLeaves: Int): Tree = {
    val (initialTree, limbLengths, attachmentPoints): (Tree, Map[Int, Int], Map[Int, AttachmentPoint]) =
      iterativelyReduceDistanceMatrix(distances, nrLeaves)
      (2 until nrLeaves).foldLeft(initialTree){
        case (tree, k) =>
          val limblength: Int = limbLengths(k)
          val AttachmentPoint(leafI, leafK, distance) = attachmentPoints(k)
          tree.insertLeafIntoTree(limblength, leafI, leafK, distance)
      }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrLeaves: Int = reader.next().toInt
    val distances: DistanceMatrix = readDistanceMatrix(reader, nrLeaves)
    val result: Tree = additivePhylogeny(distances, nrLeaves)
    result.edges.foreach(println)
  }
}
