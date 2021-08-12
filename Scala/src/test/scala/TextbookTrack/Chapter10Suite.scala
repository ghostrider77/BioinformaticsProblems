package TextbookTrack

import org.scalatest.Inspectors
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter10Suite extends AnyFreeSpec with Matchers with Inspectors {
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

  "Compute the Probability of a String Emitted by an HMM" - {
    import TextbookTrack.Chapter10.BA10D.{Label, ProbabilityMatrix, HMM, calcProbabilityOfEmittedString}

    "Should calculate the probability Pr(x) that the HMM emits x" - {
      "test case 1" in {
        val string: String = "xzyyzzyzyy"
        val states = Label(Vector('A', 'B'))
        val alphabet = Label(Vector('x', 'y', 'z'))
        val transition = ProbabilityMatrix(states, states, Vector(Vector(0.303, 0.697), Vector(0.831, 0.169)))
        val emission =
          ProbabilityMatrix(states, alphabet, Vector(Vector(0.533, 0.065, 0.402), Vector(0.342, 0.334, 0.324)))
        val hmm = HMM(alphabet, states, transition, emission)
        calcProbabilityOfEmittedString(hmm, string) shouldBe (1.1005510319694847e-06 +- 1e-12)
      }

      "test case 2" in {
        val string: String =
          "zxxxzyyxyzyxyyxzzxzyyxzzxyxxzyzzyzyzzyxxyzxxzyxxzxxyzzzzzzzxyzyxzzyxzzyzxyyyyyxzzzyzxxyyyzxyyxyzyyxz"
        val states = Label(Vector('A', 'B'))
        val alphabet = Label(Vector('x', 'y', 'z'))
        val transition = ProbabilityMatrix(states, states, Vector(Vector(0.994, 0.006), Vector(0.563, 0.437)))
        val emission =
          ProbabilityMatrix(states, alphabet, Vector(Vector(0.55, 0.276, 0.174), Vector(0.311, 0.368, 0.321)))
        val hmm = HMM(alphabet, states, transition, emission)
        calcProbabilityOfEmittedString(hmm, string) shouldBe (4.08210708381e-55 +- 1e-61)
      }
    }
  }

  "HMM Parameter Estimation Problem" - {
    import TextbookTrack.Chapter10.BA10H.{Label, HMM, estimateHmmParameters}

    "Should return a matrix of transition probabilities Transition and a matrix of emission probabilities Emission" - {
      "test case 1" in {
        val string: String = "yzzzyxzxxx"
        val alphabet = Label(Vector('x', 'y', 'z'))
        val hiddenPath: String = "BBABABABAB"
        val states = Label(Vector('A', 'B', 'C'))
        val HMM(_, _, transition, emission) = estimateHmmParameters(string, hiddenPath, alphabet, states)

        val expectedTransition: Vector[Vector[Double]] =
          Vector(Vector(0.0, 1.0, 0.0), Vector(0.8, 0.2, 0.0), Vector(0.333, 0.333, 0.333))

        forAll (transition.probabilities.toVector.zip(expectedTransition)){
          case (row, expectedRow) =>
            forAll (row.toVector.zip(expectedRow)) {
              case (p, expectedP) => p shouldBe (expectedP +- 5e-4)
            }
        }

        val expectedEmission: Vector[Vector[Double]] =
          Vector(Vector(0.25, 0.25, 0.5), Vector(0.5, 0.167, 0.333), Vector(0.333, 0.333, 0.333))

        forAll (emission.probabilities.toVector.zip(expectedEmission)){
          case (row, expectedRow) =>
            forAll (row.toVector.zip(expectedRow)) {
              case (p, expectedP) => p shouldBe (expectedP +- 5e-4)
            }
        }
      }

      "test case 2" in {
        val string: String =
          "yyzzzyzzyxyxzxyyzzzzyxyxxyyxyxzzyyyzyyyyyxxzzxxyxyxxxzxxxxxxzyzxxxzzxzyzxxxzxzxxxxzyzyxxyxzxxxxxxyxx"
        val alphabet = Label(Vector('x', 'y', 'z'))
        val hiddenPath: String =
          "BABACBABACAACBBBBCBBBCCACCABAABCAAACCACCBBBCBCBCCABBBCAABBCCABBCCBAABABACCCAACCAAABACCBCAABBCCACCABC"
        val states = Label(Vector('A', 'B', 'C'))
        val HMM(_, _, transition, emission) = estimateHmmParameters(string, hiddenPath, alphabet, states)

        val expectedTransition: Vector[Vector[Double]] =
          Vector(Vector(0.313, 0.375, 0.313), Vector(0.273, 0.364, 0.364), Vector(0.382, 0.235, 0.382))

        forAll (transition.probabilities.toVector.zip(expectedTransition)){
          case (row, expectedRow) =>
            forAll (row.toVector.zip(expectedRow)) {
              case (p, expectedP) => p shouldBe (expectedP +- 5e-4)
            }
        }

        val expectedEmission: Vector[Vector[Double]] =
          Vector(Vector(0.344, 0.406, 0.25), Vector(0.455, 0.242, 0.303), Vector(0.543, 0.2, 0.257))

        forAll (emission.probabilities.toVector.zip(expectedEmission)){
          case (row, expectedRow) =>
            forAll (row.toVector.zip(expectedRow)) {
              case (p, expectedP) => p shouldBe (expectedP +- 5e-4)
            }
        }
      }
    }
  }
}
