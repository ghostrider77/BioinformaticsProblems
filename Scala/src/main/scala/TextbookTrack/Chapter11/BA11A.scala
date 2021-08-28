package TextbookTrack.Chapter11

object BA11A {
  import TextbookTrack.Utils.IntegerMassTable

  final case class Edge(nodeFrom: Int, nodeTo: Int, label: Char)

  class Graph(initialSpectrum: List[Int]) {
    import Graph.{buildSpectrumGraph, verifySpectrum}

    val spectrum: List[Int] = verifySpectrum(initialSpectrum)
    val adjacencyList: Map[Int, List[Edge]] = buildSpectrumGraph(spectrum)

    def edges: Iterator[String] =
      for {
        (node, neighbours) <- adjacencyList.iterator
        Edge(_, neighbour, label) <- neighbours
      } yield s"$node->$neighbour:$label"
  }

  object Graph {
    private def verifySpectrum(spectrum: List[Int]): List[Int] = {
      val zeroMass: Int = 0
      val updatedSpectrum: List[Int] = if (spectrum.contains(zeroMass)) spectrum else zeroMass :: spectrum
      updatedSpectrum.sorted
    }

    private def buildSpectrumGraph(spectrum: List[Int]): Map[Int, List[Edge]] = {
      val inverseMapping: Map[Int, Char] = reverseMassTableMapping(IntegerMassTable)
      val edges: List[Edge] = for {
        (node1, ix) <- spectrum.zipWithIndex
        node2 <- spectrum.drop(ix)
        weightDifference: Int = node2 - node1
        label <- inverseMapping.get(weightDifference)
      } yield Edge(node1, node2, label)
      edges.groupBy(_.nodeFrom)
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def reverseMassTableMapping(massTable: Map[Char, Int]): Map[Int, Char] = massTable.map(_.swap)

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val spectrum: List[Int] = convertToIntList(reader.next())
    val graph = new Graph(spectrum)
    graph.edges.foreach(println)
  }
}
