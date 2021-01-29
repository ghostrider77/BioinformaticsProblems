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

  "Find a Median String" - {
    import TextbookTrack.Chapter02.BA2B.calcMedianString

    "should find a pattern that minimizes d(Pattern, texts) over all k-mers Pattern" - {
      "test case 1" in {
        val k: Int = 3
        val texts: List[String] = List("AAATTGACGCAT", "GACGACCACGTT", "CGTCAGCGCCTG", "GCTGAGCACCGG", "AGTACGGGACAG")
        Set("ACG", "GAC") should contain (calcMedianString(texts, k))
      }

      "test case 2" in {
        val k: Int = 3
        val texts: List[String] = List("ATA", "ACA", "AGA", "AAT", "AAC")
        Set("AAA") should contain (calcMedianString(texts, k))
      }
    }
  }

  "Find a Profile-most Probable k-mer in a String" - {
    import TextbookTrack.Chapter02.BA2C.{ProfileColumn, profileMostProbableKMer, readProfileMatrix}

    "should find the pattern in text that maximizes the conditional probability P(pattern|profile)" in {
      val text: String = "ACCTGTTTATTGCCTAAGTTCCGAACAAACCCAATATAGCCCGAGGGCCT"
      val k: Int = 5
      val rows: Iterator[String] =
        List(
          "0.2 0.2 0.3 0.2 0.3",
          "0.4 0.3 0.1 0.5 0.1",
          "0.3 0.3 0.5 0.2 0.4",
          "0.1 0.2 0.1 0.1 0.2"
        ).iterator
      val matrix: List[ProfileColumn] = readProfileMatrix(rows)
      profileMostProbableKMer(text, matrix, k) shouldEqual "CCGAG"
    }
  }
}
