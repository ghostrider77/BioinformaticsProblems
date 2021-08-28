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

  "Implement Viterbi Learning" - {
    import TextbookTrack.Chapter10.BA10I.{Label, HMM, ProbabilityMatrix, runViterbiLearning}

    "Should learn a matrix of transition probabilities Transition and a matrix of emission probabilities Emission" in {
      val string: String =
        "xxxzyzzxxzxyzxzxyxxzyzyzyyyyzzxxxzzxzyzzzxyxzzzxyzzxxxxzzzxyyxzzzzzyzzzxxzzxxxyxyzzyxzxxxyxzyxxyzyxz"
      val alphabet = Label(Vector('x', 'y', 'z'))
      val states = Label(Vector('A', 'B'))
      val transition = new ProbabilityMatrix(states, states, Some(Array(Array(0.582, 0.418), Array(0.272, 0.728))))
      val emission =
        new ProbabilityMatrix(states, alphabet, Some(Array(Array(0.129, 0.35, 0.52), Array(0.422, 0.151, 0.426))))
      val hmm = HMM(alphabet, states, transition, emission)
      val result: HMM = runViterbiLearning(hmm, string, n = 100)

      val expectedTransition: Vector[Vector[Double]] = Vector(Vector(0.875, 0.125), Vector(0.011, 0.989))
      forAll (result.transition.probabilities.toVector.zip(expectedTransition)){
        case (row, expectedRow) =>
          forAll (row.toVector.zip(expectedRow)) {
            case (p, expectedP) => p shouldBe (expectedP +- 5e-4)
          }
      }

      val expectedEmission: Vector[Vector[Double]] = Vector(Vector(0.0, 0.75, 0.25), Vector(0.402, 0.174, 0.424))

      forAll (result.emission.probabilities.toVector.zip(expectedEmission)){
        case (row, expectedRow) =>
          forAll (row.toVector.zip(expectedRow)) {
            case (p, expectedP) => p shouldBe (expectedP +- 5e-4)
          }
      }
    }
  }

  "Solve the Soft Decoding Problem" - {
    import TextbookTrack.Chapter10.BA10J.{Label, HMM, ProbabilityMatrix, solveSoftDecodingProblem}

    "Should calculate the probability Pr(pi = k | x_i) that the HMM was in state k at step i." - {
      "test case 1" in {
        val string: String = "zyxxxxyxzz"
        val alphabet = Label(Vector('x', 'y', 'z'))
        val states = Label(Vector('A', 'B'))
        val transition = ProbabilityMatrix(states, states, Array(Array(0.911, 0.089), Array(0.228, 0.772)))
        val emission = ProbabilityMatrix(states, alphabet, Array(Array(0.356, 0.191, 0.453), Array(0.04, 0.467, 0.493)))
        val hmm = HMM(alphabet, states, transition, emission)
        val result: Array[Array[Double]] = solveSoftDecodingProblem(hmm, string)

        val expectedConditionalProbabilities: Vector[Vector[Double]] =
          Vector(
            Vector(0.5438, 0.4562),
            Vector(0.6492, 0.3508),
            Vector(0.9647, 0.0353),
            Vector(0.9936, 0.0064),
            Vector(0.9957, 0.0043),
            Vector(0.9891, 0.0109),
            Vector(0.9154, 0.0846),
            Vector(0.964, 0.036),
            Vector(0.8737, 0.1263),
            Vector(0.8167, 0.1833)
          )

        forAll(result.toVector.zip(expectedConditionalProbabilities)) {
          case (row, expectedRow) =>
            forAll(row.toVector.zip(expectedRow)) {
              case (p, expectedP) => p shouldBe (expectedP +- 5e-5)
            }
        }
      }

      "test case 2" in {
        val string: String = "xyyzxzyxyy"
        val alphabet = Label(Vector('x', 'y', 'z'))
        val states = Label(Vector('A', 'B', 'C', 'D'))
        val transition = ProbabilityMatrix(
          rowLabels = states,
          columnLabels = states,
          probabilities = Array(
            Array(0.401, 0.009, 0.195, 0.396),
            Array(0.375, 0.237, 0.269, 0.119),
            Array(0.283, 0.25, 0.259, 0.207),
            Array(0.108, 0.529, 0.107, 0.256)
          )
        )
        val emission = ProbabilityMatrix(
          rowLabels = states,
          columnLabels = alphabet,
          probabilities = Array(
            Array(0.414, 0.335, 0.251),
            Array(0.233, 0.172, 0.596),
            Array(0.284, 0.355, 0.361),
            Array(0.028, 0.638, 0.334)
          )
        )
        val hmm = HMM(alphabet, states, transition, emission)
        val result: Array[Array[Double]] = solveSoftDecodingProblem(hmm, string)

        val expectedConditionalProbabilities: Vector[Vector[Double]] =
          Vector(
            Vector(0.5003, 0.2114, 0.2662, 0.0222),
            Vector(0.3648, 0.053, 0.1909, 0.3913),
            Vector(0.1511, 0.1251, 0.1553, 0.5685),
            Vector(0.1297, 0.5359, 0.1542, 0.1802),
            Vector(0.4414, 0.2628, 0.2673, 0.0285),
            Vector(0.3031, 0.2213, 0.2339, 0.2417),
            Vector(0.2789, 0.1536, 0.2139, 0.3537),
            Vector(0.5088, 0.269, 0.1975, 0.0247),
            Vector(0.3695, 0.0578, 0.1978, 0.3748),
            Vector(0.2231, 0.1356, 0.1658, 0.4755)
          )

        forAll(result.toVector.zip(expectedConditionalProbabilities)) {
          case (row, expectedRow) =>
            forAll(row.toVector.zip(expectedRow)) {
              case (p, expectedP) => p shouldBe (expectedP +- 5e-5)
            }
        }
      }
    }
  }

  "Implement Baum-Welch Learning" - {
    import TextbookTrack.Chapter10.BA10K.{Label, HMM, ProbabilityMatrix, runBaumWelchLearning}

    "Should calculate transition and emission probabilities that maximizes Pr(x, pi) over all possible transition " +
      "and emission matrices and over all hidden paths pi." in {
      val string: String = "xzyyzyzyxy"
      val alphabet = Label(Vector('x', 'y', 'z'))
      val states = Label(Vector('A', 'B'))
      val transition = new ProbabilityMatrix(states, states, Some(Array(Array(0.019, 0.981), Array(0.668, 0.332))))
      val emission =
        new ProbabilityMatrix(states, alphabet, Some(Array(Array(0.175, 0.003, 0.821), Array(0.196, 0.512, 0.293))))
      val hmm = HMM(alphabet, states, transition, emission)
      val result: HMM = runBaumWelchLearning(hmm, string, n = 10)

      val expectedTransition: Vector[Vector[Double]] = Vector(Vector(0.0, 1.0), Vector(0.786, 0.214))
      forAll (result.transition.probabilities.toVector.zip(expectedTransition)){
        case (row, expectedRow) =>
          forAll (row.toVector.zip(expectedRow)) {
            case (p, expectedP) => p shouldBe (expectedP +- 5e-4)
          }
      }

      val expectedEmission: Vector[Vector[Double]] = Vector(Vector(0.242, 0.0, 0.758), Vector(0.172, 0.828, 0.0))

      forAll (result.emission.probabilities.toVector.zip(expectedEmission)) {
        case (row, expectedRow) =>
          forAll(row.toVector.zip(expectedRow)) {
            case (p, expectedP) => p shouldBe (expectedP +- 5e-4)
          }
      }
    }
  }
}
