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

  "Compute the Number of Breakpoints in a Permutation" - {
    import TextbookTrack.Chapter06.BA6B.{SignedPermutation, calcNrBreakpoints, readPermutation}

    "Should return the number of breakpoints in a signed permutation" in {
      val permutation: SignedPermutation = readPermutation("(+3 +4 +5 -12 -8 -7 -6 +1 +2 +10 +9 -11 +13 +14)")
      calcNrBreakpoints(permutation) shouldEqual 8
    }
  }

  "Find All Shared k-mers of a Pair of Strings" - {
    import TextbookTrack.Chapter06.BA6E.calcSharedKMers

    "Should return the starting indices of all shared k-mers including reverse complements" in {
      val k: Int = 3
      val s1: String = "AAACTCATC"
      val s2: String = "TTTCAAATC"
      calcSharedKMers(s1, s2, k) should contain theSameElementsAs List((0, 4), (0, 0), (4, 2), (6, 6))
    }
  }

  "Implement ChromosomeToCycle" - {
    import TextbookTrack.Chapter06.BA6F.{Chromosome, Cycle, readChromosome, chromosomeToCycle}

    "Should convert a chromosome of n synteny blocks to a cycle of 2n nodes" in {
      val chromosome: Chromosome = readChromosome("(+1 -2 -3 +4)")
      val result: Cycle = chromosomeToCycle(chromosome)
      result.toString shouldEqual "(1 2 4 3 6 5 7 8)"
    }
  }
}
