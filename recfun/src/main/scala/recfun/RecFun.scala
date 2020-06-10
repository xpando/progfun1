package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    require(c >= 0, "Invalid column index")
    require(r >= 0, "Invalid row index")
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def check(chars: List[Char], open: Int): Boolean = chars match {
      case Nil => open == 0
      case '(' :: t => check(t, open + 1)
      case ')' :: t => open > 0 && check(t, open - 1)
      case _ :: t => check(t, open)
    }

    check(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    // No remaining money to be made so we have discovered 1 way
    case (0, _) => 1

    // No way to make non-zero money with no coins
    case (m, c) if m < 0 || c == Nil => 0

    // Sum all the ways with the first coin and all the ways with only the other coins
    case (m, c :: r) => countChange(m - c, coins) + countChange(m, r)
  }
}
