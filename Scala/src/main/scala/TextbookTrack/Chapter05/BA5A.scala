package TextbookTrack.Chapter05

object BA5A {
  private def convertToIntList(line: String): List[Int] = line.split(",").map(_.toInt).toList

  def calcMinimumNumberOfCoins(amount: Int, coins: List[Int]): Int = {
    val changes: Array[Int] = Array.fill(amount + 1)(0)
    (1 to amount).foreach{
      money => changes(money) = 1 + coins.withFilter(money >= _).map(coin => changes(money - coin)).min
    }
    changes(amount)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val money: Int = reader.next().toInt
    val coins: List[Int] = convertToIntList(reader.next())
    val result: Int = calcMinimumNumberOfCoins(money, coins)
    println(result)
  }
}
