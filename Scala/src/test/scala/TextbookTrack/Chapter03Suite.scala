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

  "Construct the Overlap Graph of a Collection of k-mers" - {
    import TextbookTrack.Chapter03.BA3C.OverlapGraph

    "should construct the adjacency list of an overlap graph" - {
      "test case 1" in {
        val kMers: List[String] = List("ATGCG", "GCATG", "CATGC", "AGGCA", "GGCAT")
        val graph = new OverlapGraph(kMers)
        graph.edges.toList should contain theSameElementsAs
          List("AGGCA -> GGCAT", "CATGC -> ATGCG", "GCATG -> CATGC", "GGCAT -> GCATG")
      }

      "test case 2" in {
        val kMers: List[String] = List("AAA", "AAC", "ACA", "ACG", "CAA")
        val graph = new OverlapGraph(kMers)
        graph.edges.toList should contain theSameElementsAs
          List("AAA -> AAA", "AAA -> AAC", "AAC -> ACG", "AAC -> ACA", "ACA -> CAA", "CAA -> AAA", "CAA -> AAC")
      }
    }
  }

  "Construct the De Bruijn Graph of a String" - {
    import TextbookTrack.Chapter03.BA3D.DeBruijnGraph

    def splitEdge(edge: String): List[String] = {
      val List(startNode, endNode, _*) = edge.split(" -> ").toList
      endNode.split(",").toList.map(node => s"$startNode -> $node")
    }

    "should construct the De Bruijn graph when the whole string is given" - {
      "test case 1" in {
        val k: Int = 4
        val text: String = "AAGATTCTCTAC"
        val graph = new DeBruijnGraph(text, k)
        val expectedResult: List[String] =
          List(
            "AAG -> AGA",
            "AGA -> GAT",
            "ATT -> TTC",
            "CTA -> TAC",
            "CTC -> TCT",
            "GAT -> ATT",
            "TCT -> CTA",
            "TCT -> CTC",
            "TTC -> TCT"
          )
        graph.edges.flatMap(splitEdge).toList should contain theSameElementsAs expectedResult
      }

      "test case 2" in {
        val k: Int = 3
        val text: String = "AACAAC"
        val graph = new DeBruijnGraph(text, k)
        val expectedResult: List[String] =
          List(
            "AA -> AC",
            "AA -> AC",
            "AC -> CA",
            "CA -> AA"
          )
        graph.edges.flatMap(splitEdge).toList should contain theSameElementsAs expectedResult
      }
    }
  }
}
