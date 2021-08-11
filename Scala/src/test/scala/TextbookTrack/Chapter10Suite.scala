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

  "Implement the Viterbi Algorithm" - {
    import TextbookTrack.Chapter10.BA10C.{Label, ProbabilityMatrix, HMM, calcMostLikelyHiddenPath}

    "Should calculate the path that maximizes the probability Pr(x, pi) over all possible paths pi" - {
      "test case 1" in {
        val string: String = "xyxzzxyxyy"
        val states = Label(Vector('A', 'B'))
        val alphabet = Label(Vector('x', 'y', 'z'))
        val transition = ProbabilityMatrix(states, states, Vector(Vector(0.641, 0.359), Vector(0.729, 0.271)))
        val emission =
          ProbabilityMatrix(states, alphabet, Vector(Vector(0.117, 0.691, 0.192), Vector(0.097, 0.42, 0.483)))
        val hmm = HMM(alphabet, states, transition, emission)
        calcMostLikelyHiddenPath(hmm, string) shouldEqual "AAABBAAAAA"
      }

      "test case 2" in {
        val string: String =
          "zxxxxyzzxyxyxyzxzzxzzzyzzxxxzxxyyyzxyxzyxyxyzyyyyzzyyyyzzxzxzyzzzzyxzxxxyxxxxyyzyyzyyyxzzzzyzxyzzyyy"
        val states = Label(Vector('A', 'B'))
        val alphabet = Label(Vector('x', 'y', 'z'))
        val transition = ProbabilityMatrix(states, states, Vector(Vector(0.634, 0.366), Vector(0.387, 0.613)))
        val emission =
          ProbabilityMatrix(states, alphabet, Vector(Vector(0.532, 0.226, 0.241), Vector(0.457, 0.192, 0.351)))
        val hmm = HMM(alphabet, states, transition, emission)
        calcMostLikelyHiddenPath(hmm, string) shouldEqual
          "AAAAAAAAAAAAAABBBBBBBBBBBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABBBBBBBBBBBAAAAAAAAAAAAAAAAAAAAABBBBBBBBBBAAA"
      }
    }
  }
}
