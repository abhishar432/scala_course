package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 && r == 0) 1
    else if (c < 0 || r < 0) 0
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceInt(chars: List[Char], c: Int): Boolean = {
      if (c < 0) false
      else if (chars.isEmpty) c == 0
      else {
        val hc = chars.head
        if (hc == '(') balanceInt(chars.tail, c + 1)
        else if (hc == ')') balanceInt(chars.tail, c - 1)
        else balanceInt(chars.tail, c)
      }
    }
    balanceInt(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else {
      val sortedCoins = coins.sorted
      val count = Array.fill(money + 1)(0);
      count(0) = 1
      for (den <- coins) {
        for (m <- (den to money)) {
          if (m - den >= 0)
            count(m) += count(m - den)
        }
      }
      count(money);
    }
  }
}
