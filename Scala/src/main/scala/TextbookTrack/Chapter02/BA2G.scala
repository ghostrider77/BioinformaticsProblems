package TextbookTrack.Chapter02

object BA2G {
  import scala.util.Random

  private val rng = new Random(2112)
  private val Nucleotides: Vector[Char] = Vector('A', 'C', 'G', 'T')

  final case class ProfileColumn(A: Double, C: Double, G: Double, T: Double) {
    lazy val argmax: Char = Vector(A, C, G, T).lazyZip(Nucleotides).maxBy{ case (p, _) => p }._2

    def apply(c: Char): Double = c match {
      case 'A' => A
      case 'C' => C
      case 'G' => G
      case 'T' => T
      case _ => throw new Exception("Unknown nucleotide.")
    }
  }

  object ProfileColumn {
    def apply(counts: Map[Char, Int], n: Int): ProfileColumn = {
      val adjustedProbs: Map[Char, Double] = (for {
        nucleotide <- Nucleotides
      } yield (nucleotide, (counts.getOrElse(nucleotide, 0) + 1) / (n.toDouble + 4))).toMap
      ProfileColumn(adjustedProbs('A'), adjustedProbs('C'), adjustedProbs('G'), adjustedProbs('T'))
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int, Int) = convertToIntList(line) match {
    case List(k, t, n) => (k, t, n)
    case _ => throw new Exception("Unexpected input data format.")
  }

  private def calcHammingDistance(s1: String, s2: String): Int = s1.lazyZip(s2).count { case (c1, c2) => c1 != c2 }

  private def createProfileMatrixFromMotifs(motifs: Vector[String]): Vector[ProfileColumn] =
    motifs.transpose.map{ column =>
      val counts: Map[Char, Int] = column.groupBy(identity).view.mapValues(_.length).toMap
      ProfileColumn(counts, motifs.length)
    }

  private def calcProfileMatrixScore(motifs: Vector[String]): Int = {
    val profileMatrix: Vector[ProfileColumn] = createProfileMatrixFromMotifs(motifs)
    val consensus: String = profileMatrix.map(_.argmax).mkString
    motifs.foldLeft(0)((score, motif) => score + calcHammingDistance(motif, consensus))
  }

  private def calcKMerProbabilty(kMer: String, profileMatrix: Vector[ProfileColumn]): Double =
    kMer.lazyZip(profileMatrix).foldLeft(1.0){ case (acc, (nucleotide, column)) => acc * column(nucleotide) }

  private def profileRandomlyGeneratedKMerInText(text: String, profileMatrix: Vector[ProfileColumn], k: Int): String = {
    val weights: Iterator[Double] = text.sliding(k).map(calcKMerProbabilty(_, profileMatrix))
    val cumulativeWeights: Vector[Double] = weights.scanLeft(0.0)(_ + _).drop(1).toVector
    val totalSum: Double = cumulativeWeights.last
    val r: Double = rng.nextDouble()
    val index: Int = cumulativeWeights.search(r * totalSum).insertionPoint
    text.substring(index, index + k)
  }

  private def selectRandomMotifs(texts: Vector[String], k: Int): Vector[String] =
    texts.map{ text =>
      val ix: Int = rng.between(0, text.length - k + 1)
      text.substring(ix, ix + k)
    }

  private def removeSelectedRow(motifs: Vector[String], index: Int): Vector[String] =
    motifs
      .iterator
      .zipWithIndex
      .filterNot{ case (_, ix) => ix == index }
      .map{ case (text, _) => text }
      .toVector

  private def gibbsSampling(texts: Vector[String], k: Int, t: Int, n: Int): (Vector[String], Int) = {
    val initialMotifs: Vector[String] = selectRandomMotifs(texts, k)
    val initialScore: Int = calcProfileMatrixScore(initialMotifs)
    val (bestMotif, bestScore, _): (Vector[String], Int, Vector[String]) =
      (0 until n).foldLeft((initialMotifs, initialScore, initialMotifs)){
        case ((currentBestMotif, currentBestScore, motifs), _) =>
          val ix: Int = rng.nextInt(t)
          val reducedMotifs: Vector[String] = removeSelectedRow(motifs, ix)
          val profile: Vector[ProfileColumn] = createProfileMatrixFromMotifs(reducedMotifs)
          val motif: String = profileRandomlyGeneratedKMerInText(texts(ix), profile, k)
          val updatedMotifs: Vector[String] = motifs.updated(ix, motif)
          val score: Int = calcProfileMatrixScore(updatedMotifs)
          if (score < currentBestScore) (updatedMotifs, score, updatedMotifs)
          else (currentBestMotif, currentBestScore, updatedMotifs)
      }
    (bestMotif, bestScore)
  }

  def runGibbsSampling(texts: Vector[String], k: Int, t: Int, n: Int, nrIterations: Int = 20): Vector[String] = {
    val (bestMotifs, _): (Vector[String], Int) =
      (0 until nrIterations).foldLeft((Vector.empty[String], k * texts.length)) {
        case (acc @ (_, bestScore), _) =>
          val (motifs, score): (Vector[String], Int) = gibbsSampling(texts, k, t, n)
          if (score < bestScore) (motifs, score) else acc
      }
    bestMotifs
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (k, t, n): (Int, Int, Int) = readParameters(reader.next())
    val texts: Vector[String] = reader.take(t).toVector
    val result: Vector[String] = runGibbsSampling(texts, k, t, n)
    result.foreach(println)
  }
}
