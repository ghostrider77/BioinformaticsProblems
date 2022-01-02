package AlgorithmicHeights

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AlgorithmicHeightSuite extends AnyFreeSpec with Matchers {
  object Helpers {
    def isMaxHeap(array: Array[Int], n: Int): Boolean = {
      def maxHeapProperty(ix: Int): Boolean = {
        val leftChildRelation: Boolean = if (2 * ix + 1 < n) array(ix) >= array(2 * ix + 1) else true
        val rightChildRelation: Boolean = if (2 * ix + 2 < n) array(ix) >= array(2 * ix + 2) else true
        leftChildRelation && rightChildRelation
      }

      (0 until n / 2).forall(maxHeapProperty)
    }

    def is2WayPartitioned(array: Array[Int], pivot: Int): Boolean = {
      val ix: Int = array.lastIndexOf(pivot)
      val (smaller, larger): (Array[Int], Array[Int]) = array.splitAt(ix + 1)
      smaller.forall(_ <= pivot) && larger.forall(_ > pivot)
    }

    def is3WayPartitioned(array: Array[Int], pivot: Int): Boolean = {
      val firstIx: Int = array.indexOf(pivot)
      val lastIx: Int = array.lastIndexOf(pivot)
      (array.take(firstIx).forall(_ < pivot)
        && array.slice(firstIx, lastIx + 1).forall(_ == pivot)
        && array.drop(lastIx + 1).forall(_ > pivot))
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

  "Merge Sort" - {
    import AlgorithmicHeights.MS.mergeSort

    "should return the sorted array" in {
      val xs: List[Int] = List(20, 19, 35, -18, 17, -20, 20, 1, 4, 4)
      mergeSort(xs, xs.length) shouldBe sorted
    }
  }

  "2-Way Partition" - {
    import AlgorithmicHeights.PAR.twoWayPartitioning
    import Helpers.is2WayPartitioned

    "should partition the input array according to the head as the pivot element" - {
      "test case 1" in {
        val n: Int = 9
        val array: Array[Int] = Array(7, 2, 5, 6, 1, 3, 9, 4, 8)
        val pivotElem: Int = array(0)
        twoWayPartitioning(array, pivotElem, n)
        is2WayPartitioned(array, pivotElem) shouldBe true
      }

      "test case 2" in {
        val n: Int = 7
        val array: Array[Int] = Array(7, 2, 1, 8, 7, 10, 3)
        val pivotElem: Int = array(0)
        twoWayPartitioning(array, pivotElem, n)
        is2WayPartitioned(array, pivotElem) shouldBe true
      }
    }
  }

  "3SUM" - {
    import AlgorithmicHeights.SUM3.findZeroSumIndexPairs

    "should output 3 different indices 1<=p<q<r<=n such that A[p]+A[q]+A[r]=0 if exist, and -1 otherwise" - {
      "test case 1" in {
        val arrays: List[List[Int]] =
          List(
            List(2, -3, 4, 10, 5),
            List(8, -6, 4, -2, -8),
            List(-5, 2, 3, 2, -4),
            List(2, 4, -5, 6, 8)
          )
        findZeroSumIndexPairs(arrays).map(_.toString) shouldEqual List("-1", "1 2 4", "1 2 3", "-1")
      }

      "test case 2" in {
        val arrays: List[List[Int]] = List(List(-1, 1, 1, 2))
        findZeroSumIndexPairs(arrays).map(_.toString) shouldEqual List("-1")
      }
    }
  }

  "Testing Bipartiteness" - {
    import AlgorithmicHeights.BIP.{Edge, Graph, testBipartiteness}

    "should test if an undirected simple graph is bipartite" in {
      val n1: Int = 3
      val edges1: List[Edge] = List(Edge(1, 2), Edge(3, 2), Edge(3, 1))
      val n2: Int = 4
      val edges2: List[Edge] = List(Edge(1, 4), Edge(3, 1), Edge(1, 2))
      val graphs: List[Graph] = List(new Graph(n1, edges1), new Graph(n2, edges2))
      testBipartiteness(graphs) shouldEqual List(false, true)
    }
  }

  "Testing Acyclicity" - {
    import AlgorithmicHeights.DAG.{Edge, DirectedGraph, testAcyclicity}

    "should test if a directed simple graph does not have any cycle" in {
      val graphs: List[DirectedGraph] =
        List(
          new DirectedGraph(2, List(Edge(1, 2))),
          new DirectedGraph(4, List(Edge(4, 1), Edge(1, 2), Edge(2, 3), Edge(3, 1))),
          new DirectedGraph(4, List(Edge(4, 3), Edge(3, 2), Edge(2, 1)))
        )
      testAcyclicity(graphs) shouldEqual List(true, false, true)
    }
  }

  "Dijkstra's Algorithm" - {
    import AlgorithmicHeights.DIJ.{Edge, DirectedGraph, calcShortestDistances}

    "should return the length of a shortest path from the vertex 1 to the vertex i" in {
      val edges: List[Edge] =
        List(
          Edge(3, 4, 4),
          Edge(1, 2, 4),
          Edge(1, 3, 2),
          Edge(2, 3, 3),
          Edge(6, 3, 2),
          Edge(3, 5, 5),
          Edge(5, 4, 1),
          Edge(3, 2, 1),
          Edge(2, 4, 2),
          Edge(2, 5, 3)
        )
      val graph = new DirectedGraph(6, edges)
      calcShortestDistances(graph, sourceNode = 1) shouldEqual List(0, 3, 2, 5, 6, -1)
    }
  }

  "Heap Sort" - {
    import AlgorithmicHeights.HS.heapsort

    "should sort the array in-place by using heapsort" in {
      val n: Int = 9
      val array: Array[Int] = Array(2, 6, 7, 1, 3, 5, 4, 8, 9)
      heapsort(array, n)
      array shouldBe sorted
    }
  }

  "Counting Inversions" - {
    import AlgorithmicHeights.INV.countInversions

    "should return the number of inversions in the input array" in {
      val n: Int = 6
      val xs: List[Int] = List(-6, 1, 15, 8, 10)
      val (_, result): (List[Int], Long) = countInversions(xs, n)
      result shouldEqual 2L
    }
  }

  "3-Way Partition" - {
    import AlgorithmicHeights.PAR3.threeWayPartitioning
    import Helpers.is3WayPartitioned

    "should 3-way partition the input array according to the head as the pivot element" - {
      "test case 1" in {
        val n: Int = 9
        val array: Array[Int] = Array(4, 5, 6, 4, 1, 2, 5, 7, 4)
        val pivotElem: Int = array(0)
        threeWayPartitioning(array, pivotElem, n)
        is3WayPartitioned(array, pivotElem) shouldBe true
      }

      "test case 2" in {
        val n: Int = 9
        val array: Array[Int] = Array(7, 7, 1, 7, 7, 9, 8, 2, 7)
        val pivotElem: Int = array(0)
        threeWayPartitioning(array, pivotElem, n)
        is3WayPartitioned(array, pivotElem) shouldBe true
      }
    }
  }

  "Square in a Graph" - {
    import AlgorithmicHeights.SQ.{Edge, Graph, haveGraphsSquares}

    "should check if a simple undirected graph contains a simple cycle of length 4" in {
      val graphs: List[Graph] =
        List(
          new Graph(4, List(Edge(3, 4), Edge(4, 2), Edge(3, 2), Edge(3, 1), Edge(1, 2))),
          new Graph(4, List(Edge(1, 2), Edge(3, 4), Edge(2, 4), Edge(4, 1)))
        )
      haveGraphsSquares(graphs) shouldEqual List(true, false)
    }
  }

  "Bellman-Ford Algorithm" - {
    import AlgorithmicHeights.BF.{Edge, DirectedGraph, runBellmanFordAlgorithm}

    "should return the length of a shortest paths from vertex 1 to vertex i where weights can be negative" in {
      val edges: List[Edge] =
        List(
          Edge(1, 2, 10),
          Edge(3, 2, 1),
          Edge(3, 4, 1),
          Edge(4, 5, 3),
          Edge(5, 6, -1),
          Edge(7, 6, -1),
          Edge(8, 7, 1),
          Edge(1, 8, 8),
          Edge(7, 2, -4),
          Edge(2, 6, 2),
          Edge(6, 3, -2),
          Edge(9, 5, -10),
          Edge(9, 4, 7)
        )
      val graph = new DirectedGraph(9, edges)
      val result: List[Double] = runBellmanFordAlgorithm(graph, node = 1)
      result.last.isPosInfinity shouldBe true
      result.dropRight(1).map(_.toInt) shouldEqual List(0, 5, 5, 6, 9, 7, 9,8)
    }
  }

  "Shortest Cycle Through a Given Edge" - {
    import AlgorithmicHeights.CTE.{DirectedGraph, Edge, calcShortestCycleThroughGivenEdge}

    "should output the length of a shortest cycle going through the first specified edge" in {
      val n1: Int = 4
      val edges1: List[Edge] = List(Edge(2, 4, 2), Edge(3, 2, 1), Edge(1, 4, 3), Edge(2, 1, 10), Edge(1, 3, 4))
      val n2: Int = 4
      val edges2: List[Edge] = List(Edge(3, 2, 1), Edge(2, 4, 2), Edge(4, 1, 3), Edge(2, 1, 10), Edge(1, 3, 4))
      val graphs: List[(DirectedGraph, Edge)] =
        List((new DirectedGraph(n1, edges1), edges1.head), (new DirectedGraph(n2, edges2), edges2.head))
      calcShortestCycleThroughGivenEdge(graphs) shouldEqual List(-1, 10)
    }
  }

  "Median" - {
    import AlgorithmicHeights.MED.findKthSmallestElement

    "should find the kth smallest element of an array" in {
      val n: Int = 11
      val array: Array[Int] = Array(2, 36, 5, 21, 8, 13, 11, 20, 5, 4, 1)
      (1 to n).map(findKthSmallestElement(array.clone(), n, _)).toArray shouldEqual array.sorted
    }
  }

  "Partial Sort" - {
    import AlgorithmicHeights.PS.partialSort

    "should partially sort a given array" in {
      val n: Int = 10
      val array: Array[Int] = Array(4, -6, 7, 8, -9, 100, 12, 13, 56, 17)
      val k: Int = 3
      partialSort(array, n, k) shouldEqual Vector(-9, -6, 4)
    }
  }

  "Topological Sorting" - {
    import AlgorithmicHeights.TS.{Edge, DirectedGraph}

    "should return a topological sorting of the nodes of a directed simple acyclic graph" in {
      val nrNodes: Int = 4
      val edges: List[Edge] = List(Edge(1, 2), Edge(3, 1), Edge(3, 2), Edge(4, 3), Edge(4, 2))
      val graph = new DirectedGraph(nrNodes, edges)
      graph.topologicalSorting shouldEqual List(4, 3, 1, 2)
    }
  }

  "Hamiltonian Path in DAG" - {
    import AlgorithmicHeights.HDAG.{Edge, DirectedGraph, findHamiltonianPath}

    "should return a Hamiltonian path in a directed simple acyclic graph if exists" in {
      val graph1 = new DirectedGraph(3, List(Edge(1, 2), Edge(2, 3), Edge(1, 3)))
      val graph2 = new DirectedGraph(4, List(Edge(4, 3), Edge(3, 2), Edge(4, 1)))
      findHamiltonianPath(graph1) shouldBe Some(List(1, 2, 3))
      findHamiltonianPath(graph2) shouldBe empty
    }
  }

  "Negative Weight Cycle" - {
    import AlgorithmicHeights.NWC.{Edge, DirectedGraph, hasNegativeCycle}

    "should detect if graph contains a negative cycle" - {
      "test case 1" in {
        val graph = DirectedGraph(4, List(Edge(1, 4, 4), Edge(4, 2, 3), Edge(2, 3, 1), Edge(3, 1, 6), Edge(2, 1, -7)))
        hasNegativeCycle(graph) shouldBe false
      }

      "test case 2" in {
        val graph = DirectedGraph(3, List(Edge(1, 2, -8), Edge(2, 3, 20), Edge(3, 1, -1), Edge(3, 2, -30)))
        hasNegativeCycle(graph) shouldBe true
      }

      "test case 3" in {
        val graph = DirectedGraph(3, List(Edge(1, 2, -1), Edge(2, 3, 1), Edge(3, 1, 0)))
        hasNegativeCycle(graph) shouldBe false
      }

      "test case 4" in {
        val graph = DirectedGraph(3, List(Edge(1, 2, -1), Edge(2, 3, 1), Edge(3, 1, -1)))
        hasNegativeCycle(graph) shouldBe true
      }
    }
  }

  "Quick Sort" - {
    import AlgorithmicHeights.QS.quickSort

    "should sort the array in-place by using quicksort" in {
      val n: Int = 9
      val array: Array[Int] = Array(2, 6, 7, 1, 3, 5, 4, 8, 9)
      quickSort(array, n)
      array shouldBe sorted
    }
  }

  "Strongly Connected Components" - {
    import AlgorithmicHeights.SCC.{Edge, calcStronglyConnectedComponents}

    "should retrieve the number of strongly connected components in a directed graph" in {
      val nrNodes: Int = 6
      val edges: List[Edge] = List(Edge(4, 1), Edge(1, 2), Edge(2, 4), Edge(5, 6), Edge(3, 2), Edge(5, 3), Edge(3, 5))
      calcStronglyConnectedComponents(nrNodes, edges) should have length 3
    }
  }
}
