package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter04Suite extends AnyFreeSpec with Matchers {
  "Translate an RNA String into an Amino Acid String" - {
    import TextbookTrack.Chapter04.BA4A.translateRNA

    "should translate RNA into protein" in {
      val rna: String = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
      translateRNA(rna) shouldEqual "MAMAPRTEINSTRING"
    }
  }

  "Find Substrings of a Genome Encoding a Given Amino Acid String" - {
    import TextbookTrack.Chapter04.BA4B.findPeptideEncodingSubstrings

    "should find substrings of a genome encoding a given amino acid sequence" in {
      val dna: String = "ATGGCCATGGCCCCCAGAACTGAGATCAATAGTACCCGTATTAACGGGTGA"
      val peptide: String = "MA"
      findPeptideEncodingSubstrings(dna, peptide) should contain theSameElementsAs List("ATGGCC", "GGCCAT", "ATGGCC")
    }
  }

  "Generate the Theoretical Spectrum of a Cyclic Peptide" - {
    import TextbookTrack.Chapter04.BA4C.calcTheoreticalSpectrum

    "should generate the theoretical spectrum of a cyclic peptide" in {
      val peptide: String = "LEQN"
      calcTheoreticalSpectrum(peptide) shouldEqual
        List(0, 113, 114, 128, 129, 227, 242, 242, 257, 355, 356, 370, 371, 484)
    }
  }
}
