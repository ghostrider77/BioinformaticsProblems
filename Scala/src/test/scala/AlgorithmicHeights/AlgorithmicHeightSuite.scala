package AlgorithmicHeights

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AlgorithmicHeightSuite extends AnyFreeSpec with Matchers {

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
}
