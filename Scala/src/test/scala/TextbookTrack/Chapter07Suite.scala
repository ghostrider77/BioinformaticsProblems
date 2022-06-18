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

  "Implement AdditivePhylogeny" - {
    import TextbookTrack.Chapter07.BA7C.{additivePhylogeny, Tree}

    "should construct a simple tree fitting a given additive distance matrix" - {
      "test case 1" in {
        val nrLeaves: Int = 4
        val distances: Vector[Vector[Int]] =
          Vector(Vector(0, 13, 21, 22), Vector(13, 0, 12, 13), Vector(21, 12, 0, 13), Vector(22, 13, 13, 0))
        val tree: Tree = additivePhylogeny(distances, nrLeaves)
        tree.edges.toSet shouldEqual
          Set("0->4:11", "1->4:2", "2->5:6", "3->5:7", "4->0:11", "4->1:2", "4->5:4", "5->4:4", "5->3:7", "5->2:6")
      }

      "test case 2" in {
        val nrLeaves: Int = 6
        val distances: Vector[Vector[Int]] =
          Vector(
            Vector(0, 10, 12, 13, 14, 15),
            Vector(10, 0, 6, 7, 8, 9),
            Vector(12, 6, 0, 7, 8, 9),
            Vector(13, 7, 7, 0, 9, 10),
            Vector(14, 8, 8, 9, 0, 7),
            Vector(15, 9, 9, 10, 7, 0)
          )
        val tree: Tree = additivePhylogeny(distances, nrLeaves)
        tree.edges.toSet shouldEqual
          Set(
            "0->6:8",
            "1->6:2",
            "2->7:3",
            "3->7:4",
            "4->8:3",
            "5->8:4",
            "6->0:8",
            "6->1:2",
            "6->7:1",
            "7->6:1",
            "7->2:3",
            "7->3:4",
            "7->8:2",
            "8->4:3",
            "8->5:4",
            "8->7:2"
          )
      }
    }
  }

  "Implement UPGMA" - {
    import TextbookTrack.Chapter07.BA7D.{DistanceMatrix, WeightedEdge, performUpgmaClustering, readDistanceMatrix}

    "should return the edges for the ultrametric tree output by UPGMA" in {
      val nrLeaves: Int = 4
      val lines: Iterator[String] = Iterator("0 20 17 11", "20 0 20 13", "17 20 0 10", "11 13 10 0")
      val distances: DistanceMatrix = readDistanceMatrix(lines, nrLeaves)
      val result: List[WeightedEdge] = performUpgmaClustering(distances, nrLeaves)
      val expectedResult: List[String] =
        List(
          "0->5:7.0",
          "1->6:8.833",
          "2->4:5.0",
          "3->4:5.0",
          "4->2:5.0",
          "4->3:5.0",
          "4->5:2.0",
          "5->0:7.0",
          "5->4:2.0",
          "5->6:1.833",
          "6->5:1.833",
          "6->1:8.833"
        )
      result.map(_.toString) should contain theSameElementsAs expectedResult
    }
  }

  "Implement SmallParsimony" - {
    import TextbookTrack.Chapter07.BA7F.{Node, Children, Tree, solveSmallParsimonyProblem}

    "should find the most parsimonious labeling of the internal nodes of a rooted tree" in {
      val nrLeaves: Int = 4
      val adjacencyList: Map[Node, Children] = Map(4 -> (0, 1), 5 -> (2, 3), 6 -> (4, 5))
      val characters: List[String] = List("CAAATCCC", "ATTGCGAC", "CTGCGCTG", "ATGGACGA")
      val tree = new Tree(adjacencyList, nrLeaves)
      val (parsimonyScore, _): (Int, Vector[String]) = solveSmallParsimonyProblem(tree, characters)
      parsimonyScore shouldEqual 16
    }
  }
}
