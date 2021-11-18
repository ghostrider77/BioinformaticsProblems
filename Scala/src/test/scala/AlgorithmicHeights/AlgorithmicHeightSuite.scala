package AlgorithmicHeights

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AlgorithmicHeightSuite extends AnyFreeSpec with Matchers {
  object Helpers {
    def isMaxHeap(array: Array[Int], n: Int): Boolean = {
      def maxHeapProperty(ix: Int): Boolean = {
        val leftChildRelation: Boolean = if (2*ix + 1 < n) array(ix) >= array(2*ix + 1) else true
        val rightChildRelation: Boolean = if (2*ix + 2 < n) array(ix) >= array(2*ix + 2) else true
        leftChildRelation && rightChildRelation
      }
      (0 until n / 2).forall(maxHeapProperty)
    }
  }

  "Fibonacci Numbers" - {
    import AlgorithmicHeights.FIBO.fibonacci

    "should calculate the nth Fibonacci number" in {
      val ns: List[Int] = List(0, 1, 6, 10)
      ns.map(fibonacci) shouldEqual List(0, 1, 8, 55)
    }
  }

  "Binary Search" - {
    import AlgorithmicHeights.BINS.findElemsInSortedArray

    "should find a given set of keys in a given array" in {
      val array: Vector[Int] = Vector(10, 20, 30, 40, 50)
      val queries: Vector[Int] = Vector(40, 10, 35, 15, 40, 20)
      findElemsInSortedArray(array, queries, array.length) shouldEqual Vector(4, 1, -1, -1, 4, 2)
    }
  }

  "Degree Array" - {
    import AlgorithmicHeights.DEG.{Edge, calcNodeDegrees}

    "should find the degree of all nodes in an undirected graph" in {
      val nrNodes: Int = 6
      val edges: List[Edge] = List(Edge(1, 2), Edge(2, 3), Edge(6, 3), Edge(5, 6), Edge(2, 5), Edge(2, 4), Edge(4, 1))
      calcNodeDegrees(nrNodes, edges) shouldEqual List(2, 4, 2, 2, 2, 2)
    }
  }

  "Insertion Sort" - {
    import AlgorithmicHeights.INS.calcNrSwapsInInsertionSort

    "should calculate the number of swaps performed during an insertion sort" - {
      "test case 1" in {
        val array: Array[Int] = Array(6, 10, 4, 5, 1, 2)
        calcNrSwapsInInsertionSort(array, array.length) shouldEqual 12
      }

      "test case 2" in {
        val array: Array[Int] = Array(1, 2, 4, 6, 10)
        calcNrSwapsInInsertionSort(array, array.length) shouldEqual 0
      }

      "test case 3" in {
        val array: Array[Int] = Array(6, 4, 3, 1, 0)
        calcNrSwapsInInsertionSort(array, array.length) shouldEqual 10
      }
    }
  }

  "Double-Degree Array" - {
    import AlgorithmicHeights.DDEG.{Edge, calcDoubleDegreeArray}

    "should calculate for each node the sum of degrees of its neighbours" - {
      "test case 1" in {
        val nrNodes: Int = 5
        val edges: List[Edge] = List(Edge(1, 2), Edge(2, 3), Edge(4, 3), Edge(2, 4))
        calcDoubleDegreeArray(nrNodes, edges) shouldEqual List(3, 5, 5, 5, 0)
      }

      "test case 2" in {
        val nrNodes: Int = 5
        val edges: List[Edge] = List(Edge(1, 2), Edge(2, 3), Edge(2, 5), Edge(3, 5))
        calcDoubleDegreeArray(nrNodes, edges) shouldEqual List(3, 5, 5, 0, 5)
      }
    }
  }

  "Majority Element" - {
    import AlgorithmicHeights.MAJ.calcMajorityElements

    "should output an element of an array occurring strictly more than n/2 times if such element exists" in {
      val n: Int = 8
      val arrays: List[List[Int]] =
        List(
          List(5, 5, 5, 5, 5, 5, 5, 5),
          List(8, 7, 7, 7, 1, 7, 3, 7),
          List(7, 1, 6, 5, 10, 100, 1000, 1),
          List(5, 1, 6, 7, 1, 1, 10, 1)
      )
      calcMajorityElements(arrays, n) shouldEqual List(5, 7, -1, -1)
    }
  }

  "Merge Two Sorted Arrays" - {
    import AlgorithmicHeights.MER.mergeSortedLists

    "should return a sorted array containing the elements of the two input arrays" in {
      val xs: List[Int] = List(2, 4, 10, 18)
      val ys: List[Int] = List(-5, 11, 12)
      mergeSortedLists(xs, ys) shouldEqual List(-5, 2, 4, 10, 11, 12, 18)
    }
  }

  "2SUM" - {
    import AlgorithmicHeights.SUM2.findZeroSumIndexPairs

    "should output two different indices 1<=p<q<=n such that A[p]=−A[q] if exist, and -1 otherwise" in {
      val arrays: List[List[Int]] =
        List(
          List(2, -3, 4, 10, 5),
          List(8, 2, 4, -2, -8),
          List(-5, 2, 3, 2, -4),
          List(5, 4, -5, 6, 8)
        )
      findZeroSumIndexPairs(arrays).map(_.toString) shouldEqual List("-1", "2 4", "-1", "1 3")
    }
  }

  "Breadth-First Search" - {
    import AlgorithmicHeights.BFS.{DirectedGraph, Edge}

    "should calculate the shortest paths from node 1 to any other nodes" in {
      val nrNodes: Int = 6
      val edges: List[Edge] = List(Edge(4, 6), Edge(6, 5), Edge(4, 3), Edge(3, 5), Edge(2, 1), Edge(1, 4))
      val graph = new DirectedGraph(nrNodes, edges)
      graph.breadthFirstSearch(startNode = 1) shouldEqual List(0, -1, 2, 1, 3, 2)
    }
  }

  "Connected Components" - {
    import AlgorithmicHeights.CC.{Edge, Graph}

    "should calculate the number of connected components in an udirected simple graph" - {
      "test case 1" in {
        val nrNodes: Int = 12
        val edges: List[Edge] =
          List(
            Edge(1, 2),
            Edge(1, 5),
            Edge(5, 9),
            Edge(5, 10),
            Edge(9, 10),
            Edge(3, 4),
            Edge(3, 7),
            Edge(3, 8),
            Edge(4, 8),
            Edge(7, 11),
            Edge(8, 11),
            Edge(11, 12),
            Edge(8, 12)
          )
        val graph = new Graph(nrNodes, edges)
        graph.connectedComponents should have length 3
      }

      "test case 2" in {
        val nrNodes: Int = 4
        val edges: List[Edge] =
          List(
            Edge(1, 3),
            Edge(2, 3),
            Edge(1, 4),
            Edge(2, 4)
          )
        val graph = new Graph(nrNodes, edges)
        graph.connectedComponents should have length 1
      }
    }
  }

  "Building a Heap" - {
    import AlgorithmicHeights.HEA.heapify
    import Helpers.isMaxHeap

    "should modify the input array such that the new one satisfies the max-heap property" in {
      val n: Int = 5
      val array: Array[Int] = Array(1, 3, 5, 7, 2)
      heapify(array, n)
      isMaxHeap(array, n) shouldBe true
    }
  }
}
