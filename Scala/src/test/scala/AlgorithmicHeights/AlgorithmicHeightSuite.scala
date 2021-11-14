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
}
