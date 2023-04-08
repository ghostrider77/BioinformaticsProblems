package Stronghold

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StrongholdSuite extends AnyFreeSpec with Matchers {

  "Counting DNA Nucleotides" - {
    import DNA.countNucleotides

    "should calculate the number of occurences of each nucleotide in the dna" in {
      val dna: String = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
      countNucleotides(dna) shouldEqual Vector(20, 12, 17, 21)
    }
  }

  "Transcribing DNA into RNA" - {
    import RNA.transcribeDna

    "should transcribe a Dna string to an Rna string" in {
      val dna: String = "GATGGAACTTGACTACGTAAATT"
      transcribeDna(dna) shouldEqual "GAUGGAACUUGACUACGUAAAUU"
    }
  }

  "Complementing a Strand of DNA" - {
    import REVC.calcReverseComplement

    "should calculate the reverse complement of a Dna strand" in {
      val dna: String = "AAAACCCGGT"
      calcReverseComplement(dna) shouldEqual "ACCGGGTTTT"
    }
  }
}
