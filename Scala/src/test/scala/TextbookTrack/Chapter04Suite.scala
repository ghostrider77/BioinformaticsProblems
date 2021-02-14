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
}
