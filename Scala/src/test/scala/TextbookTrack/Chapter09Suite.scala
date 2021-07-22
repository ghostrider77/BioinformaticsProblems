package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter09Suite extends AnyFreeSpec with Matchers {
  "Construct a Trie from a Collection of Patterns" - {
    import TextbookTrack.Chapter09.BA9A.Trie

    "should build a trie from a collection of words" in {
      val patterns: List[String] = List("ATAGA", "ATC", "GAT")
      val trie = new Trie(patterns)
      trie.edges().map(_.toString).toList should contain theSameElementsAs
        List("0->1:A", "1->2:T", "2->3:A", "3->4:G", "4->5:A", "2->6:C", "0->7:G", "7->8:A", "8->9:T")
    }
  }

  "Implement TrieMatching" - {
    import TextbookTrack.Chapter09.BA9B.runTrieMatching

    "should retrieve all starting positions in text where a string from patterns appears as a substring" - {
      "test case 1" in {
        val text: String = "AATCGGGTTCAATCGGGGT"
        val patterns: List[String] = List("ATCG", "GGGT")
        runTrieMatching(text, patterns) shouldEqual List(1, 4, 11, 15)
      }

      "test case 2" in {
        val text: String = "CABACD"
        val patterns: List[String] = List("A", "AB", "CD")
        runTrieMatching(text, patterns) shouldEqual List(1, 3, 4)
      }
    }
  }

  "Construct the Suffix Tree of a String" - {
    import TextbookTrack.Chapter09.BA9C.SuffixTree

    "should calculate the suffix tree of a string and print the edge labels" - {
      "test case 1" in {
        val text: String = "AATA$"
        val tree = new SuffixTree(text)
        tree.edges().toList should contain theSameElementsAs List("A", "$", "$", "TA$", "TA$", "ATA$")
      }

      "test case 2" in {
        val text: String = "ATAAATG$"
        val tree = new SuffixTree(text)
        tree.edges().toList should contain theSameElementsAs
          List("AAATG$", "G$", "T", "ATG$", "TG$", "A", "A", "AAATG$","G$", "T", "G$", "$")
      }
    }
  }

  "Find the Longest Repeat in a String" - {
    import TextbookTrack.Chapter09.BA9D.{SuffixTree, findLongestRepeatInText}

    "should calculate the longest substring of Text that appears in Text more than once" - {
      "test case 1" in {
        val text: String = "ATAAT$"
        val tree = new SuffixTree(text)
        findLongestRepeatInText(tree) shouldEqual "AT"
      }

      "test case 2" in {
        val text: String = "ATCCTCTA$"
        val tree = new SuffixTree(text)
        Set("TC", "CT") should contain (findLongestRepeatInText(tree))
      }

      "test case 3" in {
        val text: String = "ATATCGTTTTATCGTT$"
        val tree = new SuffixTree(text)
        findLongestRepeatInText(tree) shouldEqual "TATCGTT"
      }
    }
  }

  "Find the Longest Substring Shared by Two Strings" - {
    import TextbookTrack.Chapter09.BA9E.findLongestSharedSubstring

    "should calculate the longest shared substring of two strings" - {
      "test case 1" in {
        val text1: String = "ABAB"
        val text2: String = "BABA"
        val substring: String = findLongestSharedSubstring(text1, text2)

        substring should have length 3
        text1 should include (substring)
        text2 should include (substring)
      }

      "test case 2" in {
        val text1: String = "TCGGTAGATTGCGCCCACTC"
        val text2: String = "AGGGGCTCGCAGTGTAAGAA"
        val substring: String = findLongestSharedSubstring(text1, text2)

        substring should have length 3
        text1 should include (substring)
        text2 should include (substring)
      }

      "test case 3" in {
        val text1: String = "CCCCCCGGCATATATCGGCCCCCC"
        val text2: String = "GGGGGGATATATGGGGTCATCTTTTTTT"
        val substring: String = findLongestSharedSubstring(text1, text2)

        substring shouldEqual "ATATAT"
      }
    }
  }

  "Construct the Suffix Array of a String" - {
    import TextbookTrack.Chapter09.BA9G.computeSuffixArray

    "should calculate the suffix array of a string" - {
      "test case 1" in {
        val text: String = "ABABAA$"
        computeSuffixArray(text) shouldEqual Vector(6, 5, 4, 2, 0, 3, 1)
      }

      "test case 2" in {
        val text: String = "AAA$"
        computeSuffixArray(text) shouldEqual Vector(3, 2, 1, 0)
      }

      "test case 3" in {
        val text: String = "GAC$"
        computeSuffixArray(text) shouldEqual Vector(3, 1, 2, 0)
      }

      "test case 4" in {
        val text: String = "GAGAGAGA$"
        computeSuffixArray(text) shouldEqual Vector(8, 7, 5, 3, 1, 6, 4, 2, 0)
      }

      "test case 5" in {
        val text: String = "AACGATAGCGGTAGA$"
        computeSuffixArray(text) shouldEqual Vector(15, 14, 0, 1, 12, 6, 4, 2, 8, 13, 3, 7, 9, 10, 11, 5)
      }
    }
  }

  "Pattern Matching with the Suffix Array" - {
    import TextbookTrack.Chapter09.BA9H.multiplePatternMatching

    "Should find all starting positions in text where a string from Patterns appears as a substring" - {
      "test case 1" in {
        val text: String = "AATCGGGTTCAATCGGGGT$"
        val patterns: List[String] = List("ATCG", "GGGT")
        multiplePatternMatching(text, patterns) shouldEqual Set(1, 4, 11, 15)
      }

      "test case 2" in {
        val text: String = "ATATATA$"
        val patterns: List[String] = List("ATA", "ATAT")
        multiplePatternMatching(text, patterns) shouldEqual Set(0, 2, 4)
      }

      "test case 3" in {
        val text: String = "AAA"
        val patterns: List[String] = List("A")
        multiplePatternMatching(text, patterns) shouldEqual Set(0, 1, 2)
      }
    }
  }

  "Construct the Burrows-Wheeler Transform of a String" - {
    import TextbookTrack.Chapter09.BA9I.calcBurrowsWheelerTransform

    "Should construct the Burrows-Wheeler transform of a string" - {
      "test case 1" in {
        val text: String = "AA$"
        calcBurrowsWheelerTransform(text) shouldEqual "AA$"
      }

      "test case 2" in {
        val text: String = "ACACACAC$"
        calcBurrowsWheelerTransform(text) shouldEqual "CCCC$AAAA"
      }

      "test case 3" in {
        val text: String = "AGACATA$"
        calcBurrowsWheelerTransform(text) shouldEqual "ATG$CAAA"
      }

      "test case 4" in {
        val text: String = "GCGTGCCTGGTCA$"
        calcBurrowsWheelerTransform(text) shouldEqual "ACTGGCT$TGCGGC"
      }
    }
  }

  "Reconstruct a String from its Burrows-Wheeler Transform" - {
    import TextbookTrack.Chapter09.BA9J.calcInverseBurrowsWheelerTransform

    "Should reconstruct a string from its Burrows-Wheeler transform" - {
      "test case 1" in {
        val text: String = "TTCCTAACG$A"
        calcInverseBurrowsWheelerTransform(text.toList) shouldEqual "TACATCACGT$"
      }

      "test case 2" in {
        val text: String = "AGGGAA$"
        calcInverseBurrowsWheelerTransform(text.toList) shouldEqual "GAGAGA$"
      }

      "test case 3" in {
        val text: String = "AC$A"
        calcInverseBurrowsWheelerTransform(text.toList) shouldEqual "ACA$"
      }
    }
  }

  "Generate the Last-to-First Mapping of a String" - {
    import TextbookTrack.Chapter09.BA9K.calcLastToFirstMapping

    "Should calculate the position of a symbol in FirstColumn given its position at index `ix` in LastColumn" - {
      "test case 1" in {
        val transformed: String = "T$GACCA"
        val ix: Int = 3
        calcLastToFirstMapping(transformed.toList, ix) shouldEqual 1
      }

      "test case 2" in {
        val text: String = "SMNPBNNAAAAA$A"
        val ix: Int = 2
        calcLastToFirstMapping(text.toList, ix) shouldEqual 9
      }
    }
  }

  "Implement BWMatching" - {
    import TextbookTrack.Chapter09.BA9L.performPatternMatching

    "Should counts the total number of matches of pattern in text for each pattern" - {
      "test case 1" in {
        val transformed: String = "TCCTCTATGAGATCCTATTCTATGAAACCTTCA$GACCAAAATTCTCCGGC"
        val patterns: List[String] = List("CCT", "CAC", "GAG", "CAG", "ATC")
        performPatternMatching(transformed.toList, patterns) shouldEqual List(2, 1, 1, 0, 1)
      }

      "test case 2" in {
        val transformed: String = "AGGGAA$"
        val patterns: List[String] = List("GA")
        performPatternMatching(transformed.toList, patterns) shouldEqual List(3)
      }

      "test case 3" in {
        val transformed: String = "ATT$AA"
        val patterns: List[String] = List("ATA", "A", "G")
        performPatternMatching(transformed.toList, patterns) shouldEqual List(2, 3, 0)
      }

      "test case 4" in {
        val transformed: String = "AT$TCTATG"
        val patterns: List[String] = List("TCT", "TATG")
        performPatternMatching(transformed.toList, patterns) shouldEqual List(0, 0)
      }
    }
  }

  "Implement BetterBWMatching" - {
    import TextbookTrack.Chapter09.BA9M.performPatternMatching

    "Should counts the total number of matches of pattern in text for each pattern" - {
      "test case 1" in {
        val transformed: String = "TCCTCTATGAGATCCTATTCTATGAAACCTTCA$GACCAAAATTCTCCGGC"
        val patterns: List[String] = List("CCT", "CAC", "GAG", "CAG", "ATC")
        performPatternMatching(transformed.toList, patterns) shouldEqual List(2, 1, 1, 0, 1)
      }

      "test case 2" in {
        val transformed: String = "AGGGAA$"
        val patterns: List[String] = List("GA")
        performPatternMatching(transformed.toList, patterns) shouldEqual List(3)
      }

      "test case 3" in {
        val transformed: String = "ATT$AA"
        val patterns: List[String] = List("ATA", "A", "G")
        performPatternMatching(transformed.toList, patterns) shouldEqual List(2, 3, 0)
      }

      "test case 4" in {
        val transformed: String = "AT$TCTATG"
        val patterns: List[String] = List("TCT", "TATG")
        performPatternMatching(transformed.toList, patterns) shouldEqual List(0, 0)
      }

      "test case 5" in {
        val transformed: String = "GGCGCCGC$TAGTCACACACGCCGTA"
        val patterns: List[String] = List("ACC", "CCG", "CAG")
        performPatternMatching(transformed.toList, patterns) shouldEqual List(1, 2, 1)
      }
    }
  }

  "Construct the Partial Suffix Array of a String" - {
    import TextbookTrack.Chapter09.BA9Q.computePartialSuffixArray

    "Should retrieve SuffixArray_k(Text) in the form of a list of ordered pairs (i, SuffixArray(i))" in {
      val text: String = "PANAMABANANAS$"
      val k: Int = 5
      computePartialSuffixArray(text, k) shouldEqual List((1, 5), (11, 10), (12, 0))
    }
  }
}
