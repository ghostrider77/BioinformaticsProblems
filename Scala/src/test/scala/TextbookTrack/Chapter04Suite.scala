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

  "Compute the Number of Peptides of Given Total Mass" - {
    import TextbookTrack.Chapter04.BA4D.calcNumberOfPeptidesWithGivenMass

    "should comute the number of peptides given total mass when peptides are considered as integer mass sequences" - {
      "test case 1" in {
        val totalMass: Int = 1024
        calcNumberOfPeptidesWithGivenMass(totalMass) shouldEqual 14712706211L
      }

      "test case 2" in {
        val totalMass: Int = 57
        calcNumberOfPeptidesWithGivenMass(totalMass) shouldEqual 1L
      }

      "test case 3" in {
        val totalMass: Int = 113
        calcNumberOfPeptidesWithGivenMass(totalMass) shouldEqual 1L
      }

      "test case 4" in {
        val totalMass: Int = 10
        calcNumberOfPeptidesWithGivenMass(totalMass) shouldEqual 0L
      }

      "test case 5" in {
        val totalMass: Int = 0
        calcNumberOfPeptidesWithGivenMass(totalMass) shouldEqual 0L
      }
    }
  }

  "Find a Cyclic Peptide with Theoretical Spectrum Matching an Ideal Spectrum" - {
    import TextbookTrack.Chapter04.BA4E.cyclopeptideSequencing

    "should find all cyclic peptides whose theoretical spectrum matches the experimental spectrum" in {
      val spectrum: Map[Int, Int] = Map(0 -> 1, 113 -> 1, 128 -> 1, 186 -> 1, 241 -> 1, 299 -> 1, 314 -> 1, 427 -> 1)
      cyclopeptideSequencing(spectrum).map(_.toString) should contain theSameElementsAs
        List("186-128-113", "186-113-128", "128-186-113", "128-113-186", "113-186-128", "113-128-186")
    }
  }

  "Compute the Score of a Cyclic Peptide Against a Spectrum" - {
    import TextbookTrack.Chapter04.BA4F.{calcConsistencyScore, Peptide}

    "should compute the score of a cyclic peptide against an experimental spectrum" in {
      val peptide = Peptide("NQEL")
      val spectrum: Map[Int, Int] =
        Map(
          0 -> 1,
          99 -> 1,
          113 -> 1,
          114 -> 1,
          128 -> 1,
          227 -> 1,
          257 -> 1,
          299 -> 1,
          355 -> 1,
          356 -> 1,
          370 -> 1,
          371 -> 1,
          484 -> 1
        )
      calcConsistencyScore(peptide, spectrum) shouldEqual 11
    }
  }

  "Implement LeaderboardCyclopeptideSequencing" - {
    import TextbookTrack.Chapter04.BA4G.{leaderboardCyclopeptideSequencing, Peptide}

    "should find the highest scoring peptide against a given experimental spectrum" in {
      val limit: Int = 10
      val spectrum: Map[Int, Int] =
        Map(
          0 -> 1,
          71 -> 1,
          113 -> 1,
          129 -> 1,
          147 -> 1,
          200 -> 1,
          218 -> 1,
          260 -> 1,
          313 -> 1,
          331 -> 1,
          347 -> 1,
          389 -> 1,
          460 -> 1
        )
      val result: Peptide = leaderboardCyclopeptideSequencing(spectrum, limit)
      result.length shouldEqual 4
      result.masses should contain theSameElementsAs List(113, 147, 71, 129)
    }
  }

  "Generate the Convolution of a Spectrum" - {
    import TextbookTrack.Chapter04.BA4H.calcConvolutionOfSpectrum

    "Compute the convolution of a spectrum" in {
      val spectrum: Map[Int, Int] = Map(0 -> 1, 137 -> 1, 186 -> 1, 323 -> 1)
      calcConvolutionOfSpectrum(spectrum) should contain theSameElementsAs List(137, 137, 186, 186, 323, 49)
    }
  }

  "Generate the Theoretical Spectrum of a Linear Peptide" - {
    import TextbookTrack.Chapter04.BA4J.calcTheoreticalSpectrum

    "should generate the theoretical spectrum of a cyclic peptide" in {
      val peptide: String = "NQEL"
      calcTheoreticalSpectrum(peptide) shouldEqual List(0, 113, 114, 128, 129, 242, 242, 257, 370, 371, 484)
    }
  }

  "Compute the Score of a Linear Peptide" - {
    import TextbookTrack.Chapter04.BA4K.{calcConsistencyScore, Peptide}

    "should compute the linear score of a peptide against an experimental spectrum" in {
      val peptide = Peptide("NQEL")
      val spectrum: Map[Int, Int] =
        Map(
          0 -> 1,
          99 -> 1,
          113 -> 1,
          114 -> 1,
          128 -> 1,
          227 -> 1,
          257 -> 1,
          299 -> 1,
          355 -> 1,
          356 -> 1,
          370 -> 1,
          371 -> 1,
          484 -> 1
        )
      calcConsistencyScore(peptide, spectrum) shouldEqual 8
    }
  }
}
