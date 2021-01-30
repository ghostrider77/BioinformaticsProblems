package TextbookTrack.Chapter02

object BA2D {
  import scala.annotation.tailrec

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
      val probs: Map[Char, Double] =
        (for { nucleotide <- Nucleotides } yield (nucleotide, counts.getOrElse(nucleotide, 0) / n.toDouble)).toMap
      ProfileColumn(probs('A'), probs('C'), probs('G'), probs('T'))
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

  private def selectMotifFromEachText(initialMotif: String, texts: List[String], k: Int): List[String] = {
    @tailrec
    def loop(motifs: List[String], ts: List[String]): List[String] = ts match {
      case Nil => motifs.reverse
      case t :: tss =>
        val profile: List[ProfileColumn] = createProfileMatrixFromMotifs(motifs)
        val motif: String = profileMostProbableKMer(t, profile, k)
        loop(motif :: motifs, tss)
    }

    loop(List(initialMotif), texts)
  }

  def greedyMotifSearch(texts: List[String], k: Int): List[String] = texts match {
    case Nil => Nil
    case text :: rest =>
      val initialMotifs: List[String] = texts.map(_.take(k))
      val initialScore: Int = calcProfileMatrixScore(initialMotifs)
      val (bestMotifs, _): (List[String], Int) = text.sliding(k).foldLeft((initialMotifs, initialScore)){
        case (acc @ (_, bestSCore), kMer) =>
          val motifs: List[String] = selectMotifFromEachText(kMer, rest, k)
          val score: Int = calcProfileMatrixScore(motifs)
          if (score < bestSCore) (motifs, score) else acc
      }
      bestMotifs
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val (k, t): (Int, Int) = readParameters(reader.next())
    val texts: List[String] = reader.take(t).toList
    val result: List[String] = greedyMotifSearch(texts, k)
    result.foreach(println)
  }
}
