package TextbookTrack.Chapter02

object BA2F {
  import scala.annotation.tailrec
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
      } yield (nucleotide, (counts.getOrElse(nucleotide, 0) + 1) / (n.toDouble + 1))).toMap
      ProfileColumn(adjustedProbs('A'), adjustedProbs('C'), adjustedProbs('G'), adjustedProbs('T'))
    }
  }

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def readParameters(line: String): (Int, Int) = convertToIntList(line) match {
    case List(k, t) => (k, t)
    case _ => throw new Exception("Unexpected input data format.")
  }

  private def calcHammingDistance(s1: String, s2: String): Int = s1.lazyZip(s2).count { case (c1, c2) => c1 != c2 }

  private def createProfileMatrixFromMotifs(motifs: List[String]): List[ProfileColumn] =
    motifs.transpose.map{ column =>
      val counts: Map[Char, Int] = column.groupBy(identity).view.mapValues(_.length).toMap
      ProfileColumn(counts, motifs.length)
    }

  private def calcProfileMatrixScore(motifs: List[String]): Int = {
    val profileMatrix: List[ProfileColumn] = createProfileMatrixFromMotifs(motifs)
    val consensus: String = profileMatrix.map(_.argmax).mkString
    motifs.foldLeft(0)((score, motif) => score + calcHammingDistance(motif, consensus))
  }

  private def calcKMerProbabilty(kMer: String, profileMatrix: List[ProfileColumn]): Double =
    kMer.lazyZip(profileMatrix).foldLeft(1.0){ case (acc, (nucleotide, column)) => acc * column(nucleotide) }

  private def profileMostProbableKMer(text: String, profileMatrix: List[ProfileColumn], k: Int): String = {
    val (_, mostProbableKMer): (Double, String) = text.sliding(k).foldLeft((0.0, text.take(k))){
      case (acc @ (maxProbability, _), kMer) =>
        val p: Double = calcKMerProbabilty(kMer, profileMatrix)
        if (p > maxProbability) (p, kMer) else acc
    }
    mostProbableKMer
  }

  private def selectRandomMotifs(texts: List[String], k: Int): List[String] =
    texts.map{ text =>
      val ix: Int = rng.between(0, text.length - k + 1)
      text.substring(ix, ix + k)
    }

  private def randomizedMotifSearch(texts: List[String], k: Int): (List[String], Int) = {
    @tailrec
    def loop(bestMotifs: List[String], bestScore: Int, motifs: List[String]): (List[String], Int) = {
      val score: Int = calcProfileMatrixScore(motifs)
      if (score < bestScore) {
        val updatedBestScore: Int = score
        val updatedBestMotifs: List[String] = motifs
        val profile: List[ProfileColumn] = createProfileMatrixFromMotifs(motifs)
        val nextMotifs: List[String] = texts.map(profileMostProbableKMer(_, profile, k))
        loop(updatedBestMotifs, updatedBestScore, nextMotifs)
      }
      else (bestMotifs, bestScore)
    }

    val initialMotifs: List[String] = selectRandomMotifs(texts, k)
    loop(initialMotifs, k * texts.length, initialMotifs)
  }

  def runRandomizedMotifSearch(texts: List[String], k: Int, nr_iterations: Int = 1000): List[String] = {
    val (bestMotifs, _): (List[String], Int) =
      (0 until nr_iterations).foldLeft((List.empty[String], k * texts.length)) {
        case (acc @ (_, bestScore), _) =>
          val (motifs, score): (List[String], Int) = randomizedMotifSearch(texts, k)
          if (score < bestScore) (motifs, score) else acc
      }
    bestMotifs
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (k, t): (Int, Int) = readParameters(reader.next())
    val texts: List[String] = reader.take(t).toList
    val result: List[String] = runRandomizedMotifSearch(texts, k)
    result.foreach(println)
  }
}
