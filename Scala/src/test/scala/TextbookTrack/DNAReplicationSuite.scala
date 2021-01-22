package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DNAReplicationSuite extends AnyFreeSpec with Matchers {

  "k-mer problems" - {

    "Compute the Number of Times a Pattern Appears in a Text" - {
      import BA1A.countPattern

      "should return the number of times pattern appears in text" - {
        "test case 1" in {
          val text: String = "GCGCG"
          val pattern: String = "GCG"
          countPattern(text, pattern) shouldEqual 2
        }

        "test case 2" in {
          val text: String = "ACAACTATGCATACTATCGGGAACTATCCT"
          val pattern: String = "ACTAT"
          countPattern(text, pattern) shouldEqual 3
        }

        "test case 3" in {
          val text: String = "CGATATATCCATAG"
          val pattern: String = "ATA"
          countPattern(text, pattern) shouldEqual 3
        }
      }
    }

    "Find the Most Frequent Words in a String" - {
      import BA1B.mostFrequentKMer

      "should collect the most frequent k-mers from text" - {
        "test case 1" in {
          val text: String = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
          val k: Int = 4
          mostFrequentKMer(text, k) should contain theSameElementsAs List("CATG", "GCAT")
        }

        "test case 2" in {
          val text: String = "ACAACTATGCATCACTATCGGGAACTATCCT"
          val k: Int = 5
          mostFrequentKMer(text, k) should contain theSameElementsAs List("ACTAT")
        }
      }
    }

    "Find the Reverse Complement of a String" - {
      import BA1C.calcReverseComplement

      "should calculate the reverse complement of a DNA pattern" in {
        val dna: String = "AAAACCCGGT"
        calcReverseComplement(dna) shouldEqual "ACCGGGTTTT"
      }
    }

    "Implement PatternToNumber" - {
      import BA1L.patternToNumber

      "should calculate the index of a k-mer in lexicographic order" - {
        "test case 1" in {
          val pattern: String = "AGT"
          patternToNumber(pattern) shouldEqual 11
        }

        "test case 2" in {
          val pattern: String = "AAAAA"
          patternToNumber(pattern) shouldEqual 0
        }

        "test case 3" in {
          val pattern: String = "TTTTT"
          patternToNumber(pattern) shouldEqual 1023
        }
      }
    }

    "Implement NumberToPattern" - {
      import BA1M.numberToPattern

      "should calculate the k-mer based on its index in lexicographic order" - {
        "test case 1" in {
          val encoding: Int = 45
          val k: Int = 4
          numberToPattern(encoding, k) shouldEqual "AGTC"
        }

        "test case 2" in {
          val encoding: Int = 5353
          val k: Int = 7
          numberToPattern(encoding, k) shouldEqual "CCATGGC"
        }
      }
    }
  }
}
