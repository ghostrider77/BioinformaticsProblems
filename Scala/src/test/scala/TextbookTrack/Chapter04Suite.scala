package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter04Suite extends AnyFreeSpec with Matchers {
  object Fixtures {
    import TextbookTrack.Utils.readGeneticCode

    val geneticCode: Map[String, String] = readGeneticCode()
  }

  "Translate an RNA String into an Amino Acid String" - {
    import TextbookTrack.Chapter04.BA4A.translateRNA
    import Fixtures.geneticCode

    "should translate RNA into protein" in {
      val rna: String = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
      translateRNA(rna, geneticCode) shouldEqual "MAMAPRTEINSTRING"
    }
  }

  "Find Substrings of a Genome Encoding a Given Amino Acid String" - {
    import TextbookTrack.Chapter04.BA4B.findPeptideEncodingSubstrings
    import Fixtures.geneticCode

    "should find substrings of a genome encoding a given amino acid sequence" in {
      val dna: String = "ATGGCCATGGCCCCCAGAACTGAGATCAATAGTACCCGTATTAACGGGTGA"
      val peptide: String = "MA"
      findPeptideEncodingSubstrings(dna, peptide, geneticCode) should contain theSameElementsAs
        List("ATGGCC", "GGCCAT", "ATGGCC")
    }
  }
}
