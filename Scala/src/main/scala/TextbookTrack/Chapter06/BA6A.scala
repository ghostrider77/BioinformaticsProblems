package TextbookTrack.Chapter06

object BA6A {
  import scala.annotation.tailrec

  sealed trait Sign
  final case object Plus extends Sign {
    override def toString: String = "+"
  }
  final case object Minus extends Sign {
    override def toString: String = "-"
  }

  object Sign {
    def apply(char: Char): Sign = char match {
      case '+' => Plus
      case '-' => Minus
      case _ => throw new Exception("Unknow sign.")
    }
  }

  final case class SignedNumber(sign: Sign, number: Int) {
    override def toString: String = s"$sign$number"
  }

  final case class SignedPermutation(permutation: Vector[SignedNumber]) {
    override def toString: String = permutation.mkString("(", " ", ")")

    val length: Int = permutation.length

    def apply(ix: Int): SignedNumber = permutation(ix)

    def isKSorted(k: Int): Boolean = {
      val SignedNumber(sign, number) = permutation(k)
      sign == Plus && number == k + 1
    }

    def drop(k: Int): Vector[SignedNumber] = permutation.drop(k)

    def slice(a: Int, b: Int): Vector[SignedNumber] = permutation.slice(a, b)

    def take(k: Int): Vector[SignedNumber] = permutation.take(k)

    def zipWithIndex: Vector[(SignedNumber, Int)] = permutation.zipWithIndex
  }

  def readPermutation(line: String): SignedPermutation = {
    val permutations: Vector[SignedNumber] =
      line
        .dropRight(1)
        .drop(1)
        .split(" ")
        .map{ item => SignedNumber(Sign(item(0)), item.drop(1).toInt) }
        .toVector
    SignedPermutation(permutations)
  }

  private def findKInPermutationSuffix(permutation: SignedPermutation, k: Int): Option[Int] = {
    permutation
      .drop(k)
      .zipWithIndex
      .find{ case (SignedNumber(_, number), _) => number == k + 1 }
      .map{ case (_, ix) => ix + k }
  }

  private def performKReversal(permutation: SignedPermutation, k: Int): SignedPermutation = {
    findKInPermutationSuffix(permutation, k) match {
      case None => throw new Exception(s"The number $k was not found in permutation.")
      case Some(index) =>
        val kReversal: Vector[SignedNumber] =
          Iterator(permutation.take(k), permutation.slice(k, index + 1).reverse, permutation.drop(index + 1))
            .flatten
            .zipWithIndex
            .map{ case (item, ix) => if (k <= ix && ix <= index) oppositeSign(item) else item }
            .toVector
        SignedPermutation(kReversal)
    }
  }

  private def hasNegativeSign(permutation: SignedPermutation, k: Int): Boolean =
    permutation(k).sign == Minus

  private def oppositeSign(signedNumber: SignedNumber): SignedNumber =
    if (signedNumber.sign == Plus) signedNumber.copy(sign = Minus)
    else signedNumber.copy(sign = Plus)

  private def changeSign(permutation: SignedPermutation, k: Int): SignedPermutation =
    SignedPermutation(permutation.zipWithIndex.map{ case (item, ix) => if (ix == k) oppositeSign(item) else item })

  def greedySorting(permutation: SignedPermutation): List[SignedPermutation] = {
    @tailrec
    def loop(reversals: List[SignedPermutation],
             currentPermutation: SignedPermutation,
             k: Int): List[SignedPermutation] = {
      if (k == permutation.length) reversals.reverse
      else if (permutation.isKSorted(k)) loop(reversals, currentPermutation, k + 1)
      else {
        val nextPermutation: SignedPermutation = performKReversal(currentPermutation, k)
        if (hasNegativeSign(nextPermutation, k)) {
          val signCorrectedPermutation: SignedPermutation = changeSign(nextPermutation, k)
          loop(signCorrectedPermutation :: nextPermutation :: reversals, signCorrectedPermutation, k + 1)
        }
        else loop(nextPermutation :: reversals, nextPermutation, k + 1)
      }
    }

    loop(Nil, permutation, 0)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val permutation: SignedPermutation = readPermutation(reader.next())
    val result: List[SignedPermutation] = greedySorting(permutation)
    result.foreach(println)
  }
}
