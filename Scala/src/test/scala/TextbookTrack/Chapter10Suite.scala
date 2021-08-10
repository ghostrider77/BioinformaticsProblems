package TextbookTrack

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter10Suite extends AnyFreeSpec with Matchers {
  "Compute the Probability of a Hidden Path" - {
    import TextbookTrack.Chapter10.BA10A.{States, Transition, calcProbabilityOfHiddenPath}

    "Should compute the probability of a hidden path by assuming that initial probabilities are equal." in {
      val path: List[Char] = "AABBBAABABAAAABBBBAABBABABBBAABBAAAABABAABBABABBAB".toList
      val states = States(List('A', 'B'))
      val transition = Transition(states, Vector(Vector(0.194, 0.806), Vector(0.273, 0.727)))
      calcProbabilityOfHiddenPath(path, transition) shouldBe (5.01732865318e-19 +- 1e-25)
    }
  }

  "Compute the Probability of an Outcome Given a Hidden Path" - {
    import TextbookTrack.Chapter10.BA10B.{Alphabet, States, Emission, calcProbabilityOfEmittedStringGivenPath}

    "Should compute the probability of an emitted string given the hidden path of the states" in {
      val string: String = "xxyzyxzzxzxyxyyzxxzzxxyyxxyxyzzxxyzyzxzxxyxyyzxxzx"
      val path: String = "BBBAAABABABBBBBBAAAAAABAAAABABABBBBBABAABABABABBBB"
      val states = States(List('A', 'B'))
      val alphabet = Alphabet(List('x', 'y', 'z'))
      val emission = Emission(states, alphabet, Vector(Vector(0.612, 0.314, 0.074), Vector(0.346, 0.317, 0.336)))
      calcProbabilityOfEmittedStringGivenPath(string, path, emission) shouldBe (1.93157070893e-28 +- 1e-35)
    }
  }
}
