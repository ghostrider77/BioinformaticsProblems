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
}
