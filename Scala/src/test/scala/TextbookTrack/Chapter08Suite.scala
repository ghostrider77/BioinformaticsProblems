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
}
