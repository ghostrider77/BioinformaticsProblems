package TextbookTrack.Chapter04

object BA4B {
  import TextbookTrack.Utils.readGeneticCode

  private val NucleotideComplements: Map[Char, Char] = Map('A' -> 'T', 'C' -> 'G', 'T' -> 'A', 'G' -> 'C')

  private def calcReverseComplement(dna: String): String = dna.reverseIterator.map(NucleotideComplements).mkString

  private def transcriptDna(dna: String): String = dna.map(nucleotide => if (nucleotide == 'T') 'U' else nucleotide)

  def findPeptideEncodingSubstrings(dna: String, peptide: String, geneticCode: Map[String, String]): List[String] = {
    def doesKMerTranslateToPeptide(pattern: String): Boolean =
      pattern.grouped(3).map(geneticCode.get).zip(peptide).forall{
        case (Some(aminoAcid), char) => aminoAcid == char.toString
        case (None, _) => false
      }

    val length: Int = dna.length
    val forwardRna: String = transcriptDna(dna)
    val reverseRna: String = transcriptDna(calcReverseComplement(dna))
    val k: Int = 3 * peptide.length
    forwardRna.sliding(k).zipWithIndex.foldLeft(List.empty[String]){
      case (encodingSubstrings, (pattern, ix)) =>
        val reversePattern: String = reverseRna.slice(length - ix - k, length - ix)
        if (doesKMerTranslateToPeptide(pattern) || doesKMerTranslateToPeptide(reversePattern))
          dna.slice(ix, ix + k) :: encodingSubstrings
        else encodingSubstrings
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val dna: String = reader.next()
    val peptide: String = reader.next()
    val geneticCode: Map[String, String] = readGeneticCode()
    val result: List[String] = findPeptideEncodingSubstrings(dna, peptide, geneticCode)
    result.foreach(println)
  }
}
