package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter05Suite extends AnyFreeSpec with Matchers {
  "Find the Minimum Number of Coins Needed to Make Change" - {
    import TextbookTrack.Chapter05.BA5A.calcMinimumNumberOfCoins

    "should the minimum number of coins with denominations coins that changes the given amount" - {
      "test case 1" in {
        val amount: Int = 40
        val coins: List[Int] = List(1, 5, 10, 20, 25, 50)
        calcMinimumNumberOfCoins(amount, coins) shouldEqual 2
      }

      "test case 2" in {
        val amount: Int = 8074
        val coins: List[Int] = List(24, 13, 12, 7, 5, 3, 1)
        calcMinimumNumberOfCoins(amount, coins) shouldEqual 338
      }
    }
  }

  "Length of a Longest Path in the Manhattan Tourist Problem" - {
    import TextbookTrack.Chapter05.BA5B.{calcLongestPath, WeightMatrix}

    "should calculate length of a longest path from source (0, 0) to sink (n, m) in an n x m rectangular grid" in {
      val n: Int = 4
      val m: Int = 4
      val downWeights: WeightMatrix =
        Vector(Vector(1, 0, 2, 4, 3), Vector(4, 6, 5, 2, 1), Vector(4, 4, 5, 2, 1), Vector(5, 6, 8, 5, 3))
      val rightWeights: WeightMatrix =
        Vector(Vector(3, 2, 4, 0), Vector(3, 2, 4, 2), Vector(0, 7, 3, 3), Vector(3, 3, 0, 2), Vector(1, 3, 2, 2))
      calcLongestPath(n, m, downWeights, rightWeights) shouldEqual 34
    }
  }

  "Find a Longest Common Subsequence of Two Strings" - {
    import TextbookTrack.Chapter05.BA5C.calcLongestCommonSubsequence

    "should calculate the longest common subsequence of two strings" in {
      val s1: String = "AACCTTGG"
      val s2: String = "ACACTGTGA"
      val lcs: String = calcLongestCommonSubsequence(s1, s2)
      lcs should have length 6
    }
  }

  "Find a Highest-Scoring Alignment of Two Strings" - {
    import TextbookTrack.Chapter05.BA5E.calcGlobalAlignment

    "should calculate the maximum alignment score of two strings using the BLOSUM64 scoring matrix" in {
      val s1: String = "PLEASANTLY"
      val s2: String = "MEANLY"
      val sigma: Int = 5
      val (score, alignedString1, alignedString2): (Int, String, String) = calcGlobalAlignment(s1, s2, sigma)
      score shouldEqual 8
      alignedString1 shouldEqual "PLEASANTLY"
      alignedString2 shouldEqual "-MEA--N-LY"
    }
  }

  "Find a Highest-Scoring Local Alignment of Two Strings" - {
    import TextbookTrack.Chapter05.BA5F.calcLocalAlignment

    "should calculate the maximum score of a local alignment of the strings using the PAM250 scoring matrix" in {
      val s1: String = "MEANLY"
      val s2: String = "PENALTY"
      val sigma: Int = 5
      val (score, alignedString1, alignedString2): (Int, String, String) = calcLocalAlignment(s1, s2, sigma)
      score shouldEqual 15
      alignedString1 shouldEqual "EANL-Y"
      alignedString2 shouldEqual "ENALTY"
    }
  }

  "Compute the Edit Distance Between Two Strings" - {
    import TextbookTrack.Chapter05.BA5G.calcEditDistance

    "should calculate the edit distance of two strings" in {
      val s1: String = "PLEASANTLY"
      val s2: String = "MEANLY"
      calcEditDistance(s1, s2) shouldEqual 5
    }
  }
}
