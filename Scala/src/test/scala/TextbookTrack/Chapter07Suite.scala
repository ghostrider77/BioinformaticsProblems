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
}
