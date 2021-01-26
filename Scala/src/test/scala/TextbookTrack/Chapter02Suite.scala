package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter02Suite extends AnyFreeSpec with Matchers {

  "Implement MotifEnumeration" - {
    import TextbookTrack.Chapter02.BA2A.motifEnumeration

    "should return the approximate motifs in all texts" - {
      "test case 1" in {
        val k: Int = 3
        val d: Int = 1
        val texts: Set[String] = Set("ATTTGGC", "TGCCTTA", "CGGTATC", "GAAAATT")
        motifEnumeration(texts, k, d) shouldEqual Set("ATA", "ATT", "GTT", "TTT")
      }

      "test case 2" in {
        val k: Int = 3
        val d: Int = 0
        val texts: Set[String] = Set("ACGT", "ACGT", "ACGT")
        motifEnumeration(texts, k, d) shouldEqual Set("ACG", "CGT")
      }

      "test case 3" in {
        val k: Int = 3
        val d: Int = 1
        val texts: Set[String] = Set("AAAAA")
        motifEnumeration(texts, k, d) shouldEqual
          Set("AAA", "AAC", "AAG", "AAT", "ACA", "AGA", "ATA", "CAA", "GAA", "TAA")
      }
    }
  }
}
