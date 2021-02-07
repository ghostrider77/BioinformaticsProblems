package TextbookTrack

import org.scalatest.Inspectors
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter03Suite extends AnyFreeSpec with Matchers with Inspectors {

  object TestUtils {
    def splitEdge(edge: String): List[String] = {
      val List(startNode, endNode, _*) = edge.split(" -> ").toList
      endNode.split(",").toList.map(node => s"$startNode -> $node")
    }
  }

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
    import TestUtils.splitEdge

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
        graph.edges.flatMap(splitEdge).toList should contain theSameElementsAs
          List("AA -> AC", "AA -> AC", "AC -> CA", "CA -> AA")
      }
    }
  }

  "Construct the De Bruijn Graph of a Collection of k-mers" - {
    import TextbookTrack.Chapter03.BA3E.DeBruijnGraph
    import TestUtils.splitEdge

    "should construct the De Bruijn graph when a collection of k-mers is given" - {
      val kMers: List[String] = List("GAGG", "CAGG", "GGGG", "GGGA", "CAGG", "AGGG", "GGAG")
      val graph = new DeBruijnGraph(kMers)
      graph.edges.flatMap(splitEdge).toList should contain theSameElementsAs
        List("AGG -> GGG", "CAG -> AGG", "CAG -> AGG", "GAG -> AGG", "GGA -> GAG", "GGG -> GGA", "GGG -> GGG")
    }
  }

  "Find an Eulerian Cycle in a Graph" - {
    import TextbookTrack.Chapter03.BA3F.{EulerianGraph, readLines}

    def createAdjacencyListFromEulerianCycle(cycle: List[Int]): Map[Int, List[Int]] = {
      val edges: List[(Int, Int)] = (for { List(a, b) <- cycle.sliding(2) } yield (a, b)).toList
      edges.groupBy{ case (node1, _) => node1 }.view.mapValues(_.map(_._2)).toMap
    }

    "should find an Eulerian cycle in a connected and balanced directed graph" - {
      "test case 1" in {
        val edgeStrings: Iterator[String] =
          List(
            "0 -> 3",
            "1 -> 0",
            "2 -> 6,1",
            "3 -> 2",
            "4 -> 2",
            "5 -> 4",
            "6 -> 5,8",
            "7 -> 9",
            "8 -> 7",
            "9 -> 6"
          ).iterator
        val adjacencyList: Map[Int, List[Int]] = readLines(edgeStrings)
        val graph = new EulerianGraph(adjacencyList)
        val result: List[Int] = graph.findEulerianCycle()
        val edgesFromCycle: Map[Int, List[Int]] = createAdjacencyListFromEulerianCycle(result)

        edgesFromCycle.keySet shouldEqual adjacencyList.keySet

        forAll(adjacencyList.keys) {
          node => edgesFromCycle(node) should contain theSameElementsAs adjacencyList(node)
        }
      }

      "test case 2" in {
        val edgeStrings: Iterator[String] = List("0 -> 1", "1 -> 3,2", "3 -> 0", "2 -> 4", "4 -> 1").iterator
        val adjacencyList: Map[Int, List[Int]] = readLines(edgeStrings)
        val graph = new EulerianGraph(adjacencyList)
        val result: List[Int] = graph.findEulerianCycle()
        val edgesFromCycle: Map[Int, List[Int]] = createAdjacencyListFromEulerianCycle(result)

        edgesFromCycle.keySet shouldEqual adjacencyList.keySet

        forAll(adjacencyList.keys) {
          node => edgesFromCycle(node) should contain theSameElementsAs adjacencyList(node)
        }
      }
    }
  }

  "Find an Eulerian Path in a Graph" - {
    import TextbookTrack.Chapter03.BA3G.{readLines, EulerianGraph}

    def createAdjacencyListFromEulerianPath(path: List[Int]): Map[Int, List[Int]] = {
      val edges: List[(Int, Int)] = (for { List(a, b) <- path.sliding(2) } yield (a, b)).toList
      edges.groupBy{ case (node1, _) => node1 }.view.mapValues(_.map(_._2)).toMap
    }

    "should find an Eulerian path in a connected and nearly balanced directed graph" - {
      "test case 1" in {
        val edgeStrings: Iterator[String] =
          List("0 -> 2", "1 -> 3", "2 -> 1", "3 -> 0,4", "6 -> 3,7", "7 -> 8", "8 -> 9", "9 -> 6").iterator
        val adjacencyList: Map[Int, List[Int]] = readLines(edgeStrings)
        val graph = new EulerianGraph(adjacencyList)
        val result: List[Int] = graph.findEulerianPath()
        val edgesFromPath: Map[Int, List[Int]] = createAdjacencyListFromEulerianPath(result)

        edgesFromPath.keySet shouldEqual adjacencyList.keySet

        forAll(adjacencyList.keys) {
          node => edgesFromPath(node) should contain theSameElementsAs adjacencyList(node)
        }
      }

      "test case 2" in {
        val edgeStrings: Iterator[String] = List("2 -> 3", "1 -> 2").iterator
        val adjacencyList: Map[Int, List[Int]] = readLines(edgeStrings)
        val graph = new EulerianGraph(adjacencyList)
        val result: List[Int] = graph.findEulerianPath()
        result shouldEqual List(1, 2, 3)
      }
    }
  }

  "Reconstruct a String from its k-mer Composition" - {
    import TextbookTrack.Chapter03.BA3H.{DeBruijnGraph, calcStringSpelledByAGenomePath}

    "should reconstruct the genome from its k-mer compositions" in {
      val kMers: List[String] = List("CTTA", "ACCA", "TACC", "GGCT", "GCTT", "TTAC")
      val graph = DeBruijnGraph(kMers)
      val path: List[String] = graph.findEulerianPath()
      calcStringSpelledByAGenomePath(path) shouldEqual "GGCTTACCA"
    }
  }

  "Find a k-Universal Circular String" - {
    import TextbookTrack.Chapter03.BA3I.findUniversalCircularBinaryString

    "should create a circular binary universal string for a given integer k" - {
      "test case 1" in {
        val k: Int = 1
        Set("01", "10") should contain (findUniversalCircularBinaryString(k))
      }

      "test case 2" in {
        val k: Int = 2
        Set("0110", "1100", "1001", "0011") should contain (findUniversalCircularBinaryString(k))
      }
    }
  }
}
