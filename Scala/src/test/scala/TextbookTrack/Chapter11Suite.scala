package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter11Suite extends AnyFreeSpec with Matchers {
  object TestMassTable {
    val massTable: Map[Char, Int] = Map('X' -> 4, 'Z' -> 5)
  }

  "Construct the Graph of a Spectrum" - {
    import TextbookTrack.Chapter11.BA11A.Graph

    "Should compute the graph of a spectrum" in {
      val spectrum: List[Int] = List(57, 71, 154, 185, 301, 332, 415, 429, 486)
      val graph = new Graph(spectrum)
      val expectedEdges: List[String] =
        List(
          "0->57:G",
          "0->71:A",
          "57->154:P",
          "57->185:K",
          "71->185:N",
          "154->301:F",
          "185->332:F",
          "301->415:N",
          "301->429:K",
          "332->429:P",
          "415->486:A",
          "429->486:G"
      )

      graph.edges.toList should contain theSameElementsAs expectedEdges
    }
  }

  "Implement DecodingIdealSpectrum" - {
    import TextbookTrack.Chapter11.BA11B.decodeAnIdealSpectrum

    "Should compute the graph of a spectrum" in {
      val spectrum: List[Int] = List(57, 71, 154, 185, 301, 332, 415, 429, 486)
      val result: Option[String] = decodeAnIdealSpectrum(spectrum)
      Set(Some("GPFNA"), Some("ANFPG")) should contain (result)
    }
  }

  "Convert a Peptide into a Peptide Vector" - {
    import TextbookTrack.Chapter11.BA11C.calcPeptideVector
    import TestMassTable.massTable

    "Should calculate the peptide vector of a peptide" in {
      val peptide: String = "XZZXX"
      val result: List[Int] = calcPeptideVector(peptide, massTable)
      result shouldEqual List(0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1)
    }
  }

  "Convert a Peptide Vector into a Peptide" - {
    import TextbookTrack.Chapter11.BA11D.restorePeptideFromPeptideVector
    import TestMassTable.massTable

    "Should recreate the peptide form the peptide vector" in {
      val peptideVector: List[Int] = List(0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1)
      val result: Option[String] = restorePeptideFromPeptideVector(peptideVector, massTable)
      result shouldBe Some("XZZXX")
    }
  }

  "Sequence a Peptide" - {
    import TextbookTrack.Chapter11.BA11E.runPeptideSequencing
    import TestMassTable.massTable

    "Should return a peptide with maximum score against spectrum" in {
      val spectrum: List[Int] = List(0, 0, 0, 4, -2, -3, -1, -7, 6, 5, 3, 2, 1, 9, 3, -8, 0, 3, 1, 2, 1, 0)
      val result: String = runPeptideSequencing(spectrum, massTable)
      result shouldEqual "XZZXX"
    }
  }

  "Find a Highest-Scoring Peptide in a Proteome against a Spectrum" - {
    import TextbookTrack.Chapter11.BA11F.findHighestScoringPeptideInProteome
    import TestMassTable.massTable

    "Should find a peptide from a proteome with maximum score against a spectrum" in {
      val spectrum: Vector[Int] = Vector(0, 0, 0, 4, -2, -3, -1, -7, 6, 5, 3, 2, 1, 9, 3, -8, 0, 3, 1, 2, 1, 8)
      val proteome: String = "XZZXZXXXZXZZXZXXZ"
      val result: String = findHighestScoringPeptideInProteome(spectrum, proteome, massTable)
      result shouldEqual "ZXZXX"
    }
  }

  "Implement PSMSearch" - {
    import TextbookTrack.Chapter11.BA11G.findPeptideSpectrumMatchPairs
    import TestMassTable.massTable

    "Should find all unique peptide-spectrum matches scoring at least as high as threshold" in {
      val spectra: Vector[Vector[Int]] =
        Vector(
          Vector(-1, 5, -4, 5, 3, -1, -4, 5, -1, 0, 0, 4, -1, 0, 1, 4, 4, 4),
          Vector(-4, 2, -2, -4, 4, -5, -1, 4, -1, 2, 5, -3, -1, 3, 2, -3)
        )
      val proteome: String = "XXXZXZXXZXZXXXZXXZX"
      val threshold: Int = 5
      val result: Set[String] = findPeptideSpectrumMatchPairs(spectra, proteome, threshold, massTable)
      result shouldEqual Set("XZXZ")
    }
  }

  "Compute the Size of a Spectral Dictionary" - {
    import TextbookTrack.Chapter11.BA11H.calcSpectralDictionarySize
    import TestMassTable.massTable

    "Should find the size of the spectral dictionary for a given spectrum and score threshold" in {
      val spectrum: Vector[Int] = Vector(4, -3, -2, 3, 3, -4, 5, -3, -1, -1, 3, 4, 1, 3)
      val threshold: Int = 1
      val maxScore: Int = 8
      val result: Int = calcSpectralDictionarySize(spectrum, massTable, threshold, maxScore)
      result shouldEqual 3
    }
  }

  "Compute the Probability of a Spectral Dictionary" - {
    import TextbookTrack.Chapter11.BA11I.calcProbabilityOfSpectralDictionary
    import TestMassTable.massTable

    "Should find the size of the spectral dictionary for a given spectrum and score threshold" in {
      val spectrum: Vector[Int] = Vector(4, -3, -2, 3, 3, -4, 5, -3, -1, -1, 3, 4, 1, 3)
      val threshold: Int = 1
      val maxScore: Int = 8
      val result: Double = calcProbabilityOfSpectralDictionary(spectrum, massTable, threshold, maxScore)
      result shouldBe (0.375 +- 1e-8)
    }
  }

  "Find a Highest-Scoring Modified Peptide against a Spectrum" - {
    import TextbookTrack.Chapter11.BA11J.solveSpectralAlignmentProblem
    import TestMassTable.massTable

    "Should find a modified variant of a peptide that maximizes the peptide-spectrum score among all variants of the " +
      "peptides with up to k modifications" in {
      val peptide: String = "XXZ"
      val spectrum: Vector[Int] = Vector(4, -3, -2, 3, 3, -4, 5, -3, -1, -1, 3, 4, 1, 3)
      val k: Int = 2
      solveSpectralAlignmentProblem(peptide, spectrum, massTable, k) shouldEqual "XX(-1)Z(+2)"
    }
  }
}
