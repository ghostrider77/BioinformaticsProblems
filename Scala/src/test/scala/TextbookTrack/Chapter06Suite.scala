package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter06Suite extends AnyFreeSpec with Matchers {
  "Implement GreedySorting to Sort a Permutation by Reversals" - {
    import TextbookTrack.Chapter06.BA6A.{SignedPermutation, greedySorting, readPermutation}

    "Should return the sequence of permutations corresponding to greedy sorting" in {
      val permutation: SignedPermutation = readPermutation("(-3 +4 +1 +5 -2)")
      val result: List[SignedPermutation] = greedySorting(permutation)
      result.map(_.toString) shouldEqual
        List(
          "(-1 -4 +3 +5 -2)",
          "(+1 -4 +3 +5 -2)",
          "(+1 +2 -5 -3 +4)",
          "(+1 +2 +3 +5 +4)",
          "(+1 +2 +3 -4 -5)",
          "(+1 +2 +3 +4 -5)",
          "(+1 +2 +3 +4 +5)"
        )
    }
  }
}
