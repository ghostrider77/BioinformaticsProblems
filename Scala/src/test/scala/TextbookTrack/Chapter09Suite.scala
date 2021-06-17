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
}
