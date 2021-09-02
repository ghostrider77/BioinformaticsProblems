package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter11Suite extends AnyFreeSpec with Matchers {
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
}
