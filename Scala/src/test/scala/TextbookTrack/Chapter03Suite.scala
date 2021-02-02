package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter03Suite extends AnyFreeSpec with Matchers {

  "Generate the k-mer Composition of a String" - {
    import TextbookTrack.Chapter03.BA3A.calcKMerComposition

    "should generate the lexicographically ordered k-mer composition of a string" in {
      val k: Int = 5
      val text: String = "CAATCCAAC"
      calcKMerComposition(text, k) shouldEqual List("AATCC", "ATCCA", "CAATC", "CCAAC", "TCCAA")
    }
  }

  "Reconstruct a String from its Genome Path" - {
    import TextbookTrack.Chapter03.BA3B.calcStringSpelledByAGenomePath

    "should find the string spelled by a genome path" in {
      val kMers: List[String] = List("ACCGA", "CCGAA", "CGAAG", "GAAGC", "AAGCT")
      calcStringSpelledByAGenomePath(kMers) shouldEqual "ACCGAAGCT"
    }
  }
}
