package TextbookTrack

import org.scalatest.Inspectors
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter08Suite extends AnyFreeSpec with Matchers with Inspectors {
  "Implement FarthestFirstTraversal" - {
    import TextbookTrack.Chapter08.BA8A.{Point, farthestFirstTraversal}

    "should calculate k centers based on the Farthest First Traversal algorithm" in {
      val k: Int = 3
      val points: List[Point] =
        List(
          List(0.0, 0.0),
          List(5.0, 5.0),
          List(0.0, 5.0),
          List(1.0, 1.0),
          List(2.0, 2.0),
          List(3.0, 3.0),
          List(1.0, 2.0)
        )
      farthestFirstTraversal(points, k) shouldEqual List(List(0.0, 0.0), List(5.0, 5.0), List(0.0, 5.0))
    }
  }

  "Compute the Squared Error Distortion" - {
    import TextbookTrack.Chapter08.BA8B.{Point, calcDistortion}

    "should calculate the squared error distortion" in {
      val centers: List[Point] = List(List(2.31, 4.55), List(5.96, 9.08))
      val points: List[Point] =
        List(
          List(3.42, 6.03),
          List(6.23, 8.25),
          List(4.76, 1.64),
          List(4.47, 4.33),
          List(3.95, 7.61),
          List(8.93, 2.97),
          List(9.74, 4.03),
          List(1.73, 1.28),
          List(9.72, 5.01),
          List(7.27, 3.77)
      )
      calcDistortion(points, centers) shouldBe (18.246 +- 1e-3)
    }
  }

  "Implement the Lloyd Algorithm for k-Means Clustering" - {
    import TextbookTrack.Chapter08.BA8C.{Point, runKMeansClustering}

    "should return k centers calculated by the Lloyd algorithm" in {
      val k: Int = 2
      val points: List[Point] =
        List(
          List(1.3, 1.1),
          List(1.3, 0.2),
          List(0.6, 2.8),
          List(3.0, 3.2),
          List(1.2, 0.7),
          List(1.4, 1.6),
          List(1.2, 1.0),
          List(1.2, 1.1),
          List(0.6, 1.5),
          List(1.8, 2.6),
          List(1.2, 1.3),
          List(1.2, 1.0),
          List(0.0, 1.9)
        )
      val expectedResult: List[Point] = List(List(1.800, 2.867), List(1.060, 1.140))
      val result: List[Point] = runKMeansClustering(points, k)

      forAll(result.zip(expectedResult)) {
        case (calculatedCenter, expectedCenter) =>
          forAll(calculatedCenter.zip(expectedCenter)) {
            case (p, q) => p shouldBe (q +- 1e-3)
          }
      }
    }
  }

  "Implement the Soft k-Means Clustering Algorithm" - {
    import TextbookTrack.Chapter08.BA8D.{Point, runSoftKMeansClustering}

    "should return k centers calculated by the soft k-means algorithm" in {
      val k: Int = 2
      val beta: Double = 2.7
      val points: List[Point] =
        List(
          List(1.3, 1.1),
          List(1.3, 0.2),
          List(0.6, 2.8),
          List(3.0, 3.2),
          List(1.2, 0.7),
          List(1.4, 1.6),
          List(1.2, 1.0),
          List(1.2, 1.1),
          List(0.6, 1.5),
          List(1.8, 2.6),
          List(1.2, 1.3),
          List(1.2, 1.0),
          List(0.0, 1.9)
        )
      val expectedResult: List[Point] = List(List(1.662, 2.623), List(1.075, 1.148))
      val result: List[Point] = runSoftKMeansClustering(points, k, beta)

      forAll(result.zip(expectedResult)) {
        case (calculatedCenter, expectedCenter) =>
          forAll(calculatedCenter.zip(expectedCenter)) {
            case (p, q) => p shouldBe (q +- 1e-3)
          }
      }
    }
  }

  "Implement Hierarchical Clustering" - {
    import TextbookTrack.Chapter08.BA8E.{Cluster, DistanceMatrix, readDistanceMatrix, runHierarchicalClustering}

    "should apply hierarchical clustering to a distance matrix using D_{avg}" in {
      val n: Int = 7
      val lines: Iterator[String] =
        Iterator(
          "0.00 0.74 0.85 0.54 0.83 0.92 0.89",
          "0.74 0.00 1.59 1.35 1.20 1.48 1.55",
          "0.85 1.59 0.00 0.63 1.13 0.69 0.73",
          "0.54 1.35 0.63 0.00 0.66 0.43 0.88",
          "0.83 1.20 1.13 0.66 0.00 0.72 0.55",
          "0.92 1.48 0.69 0.43 0.72 0.00 0.80",
          "0.89 1.55 0.73 0.88 0.55 0.80 0.00"
        )
      val distanceMatrix: DistanceMatrix = readDistanceMatrix(lines, n)
      val result: List[Cluster] = runHierarchicalClustering(distanceMatrix, n)
      val clusterIds = result.map(_.content.map(_ + 1))
      clusterIds shouldEqual List(
        Vector(4, 6),
        Vector(5, 7),
        Vector(3, 4, 6),
        Vector(1, 2),
        Vector(5, 7, 3, 4, 6),
        Vector(1, 2, 5, 7, 3, 4, 6)
      )
    }
  }
}
