package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter01Suite extends AnyFreeSpec with Matchers {

  "k-mer problems" - {

    "Compute the Number of Times a Pattern Appears in a Text" - {
      import TextbookTrack.Chapter01.BA1A.countPattern

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
      import TextbookTrack.Chapter01.BA1B.mostFrequentKMer

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
      import TextbookTrack.Chapter01.BA1C.calcReverseComplement

      "should calculate the reverse complement of a DNA pattern" in {
        val dna: String = "AAAACCCGGT"
        calcReverseComplement(dna) shouldEqual "ACCGGGTTTT"
      }
    }

    "Find All Occurrences of a Pattern in a String" - {
      import TextbookTrack.Chapter01.BA1D.findPatternOccurrences

      "should calculate the indices where pattern appears in text" - {
        "test case 1" in {
          val text: String = "GATATATGCATATACTT"
          val pattern: String = "ATAT"
          findPatternOccurrences(text, pattern) should contain theSameElementsAs List(1, 3, 9)
        }

        "test case 2" in {
          val text: String = "ATATATA"
          val pattern: String = "ATA"
          findPatternOccurrences(text, pattern) should contain theSameElementsAs List(0, 2, 4)
        }
      }
    }

    "Find Patterns Forming Clumps in a String" - {
      import TextbookTrack.Chapter01.BA1E.findClumpsInText

      "should find k-mers forming (l, t)-clumps in a string" - {
        "test case 1" in {
          val text: String = "ACGTACGT"
          val k: Int = 1
          val l: Int = 5
          val t: Int = 2
          findClumpsInText(text, k, l, t) should contain theSameElementsAs List("A", "C", "G", "T")
        }

        "test case 2" in {
          val text: String = "AAAACGTCGAAAAA"
          val k: Int = 2
          val l: Int = 4
          val t: Int = 2
          findClumpsInText(text, k, l, t) should contain theSameElementsAs List("AA")
        }

        "test case 3" in {
          val text: String =
            "CGGACTCGACAGATGTGAAGAAATGTGAAGACTGAGTGAAGAGAAGAGGAAACACGACACGACATTGCGACATAATGTACGAATGTAATGTGCCTATGGC"
          val k: Int = 5
          val l: Int = 75
          val t: Int = 4
          findClumpsInText(text, k, l, t) should contain theSameElementsAs List("CGACA", "GAAGA", "AATGT")
        }

        "test case 4" in {
          val text: String = "CGGACTCGACAGATGTGAAGAACGACAATGTGAAGACTCGACACGACAGAGTGAAGAGAAGAGGAAACATTGTAA"
          val k: Int = 5
          val l: Int = 50
          val t: Int = 4
          findClumpsInText(text, k, l, t) should contain theSameElementsAs List("CGACA", "GAAGA")
        }
      }
    }

    "Find a Position in a Genome Minimizing the Skew" - {
      import TextbookTrack.Chapter01.BA1F.getSkewnessArgmins

      "should calculate the minimum indices of GC-skewness" - {
        "test case 1" in {
          val genome: String =
            "CCTATCGGTGGATTAGCATGTCCCTGTACGTTTCGCCGCGAACTAGTTCACACGGCTTGATGGCAAATGGTTTTTCCGGCGACCGTAATCGTCCACCGAG"
          getSkewnessArgmins(genome) shouldEqual List(53, 97)
        }

        "test case 2" in {
          val genome: String = "TAAAGACTGCCGAGAGGCCAACACGAGTGCTAGAACGAGGGGCGTAAACGCGGGTCCGAT"
          getSkewnessArgmins(genome) shouldEqual List(11, 24)
        }
      }
    }

    "Compute the Hamming Distance Between Two Strings" - {
      import TextbookTrack.Chapter01.BA1G.calcHammingDistance

      "should calculate the Hamming distance" in {
        val s1: String = "GGGCCGTTGGT"
        val s2: String = "GGACCGTTGAC"
        calcHammingDistance(s1, s2) shouldEqual 3
      }
    }

    "Find All Approximate Occurrences of a Pattern in a String" - {
      import TextbookTrack.Chapter01.BA1H.findApproximatePatternOccurrences

      "should calculate the indices where pattern approximately appears in text up to given Hamming distance" - {
        "test case 1" in {
          val text: String =
            "CGCCCGAATCCAGAACGCATTCCCATATTTCGGGACCACTGGCCTCCACGGTACGGACGTCAATCAAATGCCTAGCGGCTTGTGGTTTCTCCTACGCTCC"
          val pattern: String = "ATTCTGGA"
          val d: Int = 3
          findApproximatePatternOccurrences(text, pattern, d) should contain theSameElementsAs List(6, 7, 26, 27, 78)
        }

        "test case 2" in {
          val text: String = "CCACCT"
          val pattern: String = "CCA"
          val d: Int = 0
          findApproximatePatternOccurrences(text, pattern, d) should contain theSameElementsAs List(0)
        }

        "test case 3" in {
          val text: String = "AAAAAA"
          val pattern: String = "TTT"
          val d: Int = 3
          findApproximatePatternOccurrences(text, pattern, d) should contain theSameElementsAs List(0, 1, 2, 3)
        }
      }
    }

    "Generate the Frequency Array of a String" - {
      import TextbookTrack.Chapter01.BA1K.computingFrequencies

      "should calculate the k-mer frequency array of a string" - {
        "test case 1" in {
          val text: String = "ACGCGGCTCTGAAA"
          val k: Int = 2
          computingFrequencies(text, k) shouldEqual Vector(2, 1, 0, 0, 0, 0, 2, 2, 1, 2, 1, 0, 0, 1, 1, 0)
        }

        "test case 2" in {
          val text: String = "AAAAC"
          val k: Int = 2
          computingFrequencies(text, k) shouldEqual Vector(3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        }
      }
    }

    "Implement PatternToNumber" - {
      import TextbookTrack.Chapter01.BA1L.patternToNumber

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
      import TextbookTrack.Chapter01.BA1M.numberToPattern

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

    "Generate the d-Neighborhood of a String" - {
      import TextbookTrack.Chapter01.BA1N.generateNeighbourhood

      "should retrieve strings that are at most d Hamming distance away from a given string" - {
        "test case 1" in {
          val pattern: String = "ACG"
          val d: Int = 1
          generateNeighbourhood(pattern, d) shouldEqual
            Set("CCG", "TCG", "GCG", "AAG", "ATG", "AGG", "ACA", "ACC", "ACT", "ACG")
        }

        "test case 2" in {
          val pattern: String = "A"
          val d: Int = 10
          generateNeighbourhood(pattern, d) shouldEqual Set("A", "C", "G", "T")
        }
      }
    }
  }
}
