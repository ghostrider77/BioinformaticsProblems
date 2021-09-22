package TextbookTrack.Chapter06

object BA6B {
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

    val value: Int = if (sign == Plus) number else -number

    def toInt: Int = value

    def +(that: SignedNumber): SignedNumber = {
      val result: Int = this.value + that.value
      if (result < 0) SignedNumber(Minus, math.abs(result))
      else SignedNumber(Plus, result)
    }

    def unary_- : SignedNumber = if (sign == Plus) SignedNumber(Minus, number) else SignedNumber(Plus, number)

    def -(that: SignedNumber): SignedNumber = this + (-that)
  }

  final case class SignedPermutation(permutation: Vector[SignedNumber]) {
    override def toString: String = permutation.mkString("(", " ", ")")

    val length: Int = permutation.length

    def apply(ix: Int): SignedNumber = permutation(ix)
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

  private def createExtendedPermutation(permutation: SignedPermutation): SignedPermutation = {
    val extendedPermutation: Vector[SignedNumber] =
      (-1 to permutation.length)
        .map{ ix =>
          if (ix == -1) SignedNumber(Plus, 0)
          else if (ix == permutation.length) SignedNumber(Plus, permutation.length + 1)
          else permutation(ix)
        }
        .toVector
    SignedPermutation(extendedPermutation)
  }

  def calcNrBreakpoints(permutation: SignedPermutation): Int = {
    val extendedPermutation: SignedPermutation = createExtendedPermutation(permutation)
    extendedPermutation
      .permutation
      .iterator
      .sliding(2)
      .collect{ case Seq(a, b) => (a, b) }
      .count{ case (a, b) => (b - a).toInt != 1 }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val permutation: SignedPermutation = readPermutation(reader.next())
    val result: Int = calcNrBreakpoints(permutation)
    println(result)
  }
}
