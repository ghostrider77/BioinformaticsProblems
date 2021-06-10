package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter08Suite extends AnyFreeSpec with Matchers {
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
}
