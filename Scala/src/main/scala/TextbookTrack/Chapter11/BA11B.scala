package TextbookTrack.Chapter11

object BA11B {
  import scala.language.implicitConversions
  import TextbookTrack.Utils.IntegerMassTable

  final case class Edge(nodeFrom: Int, nodeTo: Int, label: Char)

  class Graph(spectrum: List[Int], inverseMassTable: Map[Int, Char]) {
    import Graph.buildSpectrumGraph

    val adjacencyList: Map[Int, List[Edge]] = buildSpectrumGraph(spectrum, inverseMassTable)

    def findAllPathsToSink(v: Int, sink: Int): Set[List[Int]] = {
      if (v == sink) Set(List(v))
      else {
        val neighbours: List[Edge] = adjacencyList.getOrElse(v, Nil)
        neighbours.foldLeft(Set.empty[List[Int]]){
          case (paths, Edge(_, w, _)) =>
            val pathsFromWToSink: Set[List[Int]] = findAllPathsToSink(w, sink)
            paths ++ pathsFromWToSink.map(v :: _)
        }
      }
    }
  }

  object Graph {
    private def buildSpectrumGraph(spectrum: List[Int], inverseMassTable: Map[Int, Char]): Map[Int, List[Edge]] = {
      val edges: List[Edge] = for {
        (node1, ix) <- spectrum.zipWithIndex
        node2 <- spectrum.drop(ix)
        weightDifference: Int = node2 - node1
        label <- inverseMassTable.get(weightDifference)
      } yield Edge(node1, node2, label)
      edges.groupBy(_.nodeFrom)
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def reverseMassTableMapping(massTable: Map[Char, Int]): Map[Int, Char] = massTable.map(_.swap)

  private def verifySpectrum(spectrum: List[Int]): List[Int] = {
    val zeroMass: Int = 0
    val updatedSpectrum: List[Int] = if (spectrum.contains(zeroMass)) spectrum else zeroMass :: spectrum
    updatedSpectrum.sorted
  }

  private implicit def toMassCounts(masses: List[Int]): Map[Int, Int] =
    masses.groupMapReduce(identity)(_ => 1)(_ + _)

  private def calcIdealSpectrum(peptide: List[Int]): List[Int] = {
    val prefixMasses: List[Int] = peptide.scanLeft(0)(_ + _)
    val suffixMasses: List[Int] = peptide.scanRight(0)(_ + _)
    prefixMasses.tail ++ suffixMasses.tail
  }

  private def calcPeptideSpelledByPath(path: List[Int]): List[Int] =
    (for { List(mass1, mass2) <- path.sliding(2) } yield mass2 - mass1).toList

  private def findPeptideCorrespondingToSpectrum(paths: Set[List[Int]],
                                                 spectrum: List[Int],
                                                 inverseMassTable: Map[Int, Char]): Option[String] = {
    val massCountsInSpectrum: Map[Int, Int] = spectrum
    paths
      .iterator
      .map(calcPeptideSpelledByPath)
      .find { peptideMasses =>
        val idealSpectrum: Map[Int, Int] = calcIdealSpectrum(peptideMasses)
        idealSpectrum == massCountsInSpectrum
      }
      .map(_.map(inverseMassTable).mkString)
  }

  def decodeAnIdealSpectrum(initialSpectrum: List[Int]): Option[String] = {
    val spectrum: List[Int] = verifySpectrum(initialSpectrum)
    val inverseMapping: Map[Int, Char] = reverseMassTableMapping(IntegerMassTable)
    val graph = new Graph(spectrum, inverseMapping)
    val sink: Int = spectrum.max
    val paths: Set[List[Int]] = graph.findAllPathsToSink(0, sink)
    findPeptideCorrespondingToSpectrum(paths, spectrum, inverseMapping)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val spectrum: List[Int] = convertToIntList(reader.next())
    val result: Option[String] = decodeAnIdealSpectrum(spectrum)
    result match {
      case Some(peptide) => println(peptide)
      case None => println("No peptide is encoded by the given spectrum.")
    }
  }
}
