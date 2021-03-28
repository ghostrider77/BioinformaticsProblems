package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter07Suite extends AnyFreeSpec with Matchers {
  "Compute Distances Between Leaves" - {
    import TextbookTrack.Chapter07.BA7A.{Tree, WeightedEdge, computeDistancesBetweenLeaves, readWeightedEdges}

    "should calculate the distance matrix between the leaves of a tree" in {
      val nrLeaves: Int = 4
      val edges: Iterator[String] =
        Iterator("0->4:11", "1->4:2", "2->5:6", "3->5:7", "4->0:11", "4->1:2", "4->5:4", "5->4:4", "5->3:7", "5->2:6")
      val adjacencyList: Map[Int, List[WeightedEdge]] = readWeightedEdges(edges)
      val tree = new Tree(adjacencyList, nrLeaves)
      computeDistancesBetweenLeaves(tree) shouldEqual
        List(List(0, 13, 21, 22), List(13, 0, 12, 13), List(21, 12, 0, 13), List(22, 13, 13, 0))
    }
  }

  "Compute Limb Lengths in a Tree" - {
    import TextbookTrack.Chapter07.BA7B.calcLimbLength

    "should calculate the limb length of the leaf in Tree(D) using the distance matrix" in {
      val nrLeaves: Int = 4
      val leaf: Int = 1
      val distances: Vector[Vector[Int]] =
        Vector(Vector(0, 13, 21, 22), Vector(13, 0, 12, 13), Vector(21, 12, 0, 13), Vector(22, 13, 13, 0))
      calcLimbLength(distances, leaf, nrLeaves) shouldEqual 2
    }
  }
}
