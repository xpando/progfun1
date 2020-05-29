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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

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
    case (_, Nil) => 0         // Can't make change with no coins
    case (m, _) if m < 0 => 0  // Can't make change for negative money (likely overshot with recusive call)
    case (0, _) => 1           // Terminal case. Recursive calls up to hear represent one of the ways to make change
    case (m, c :: r) => countChange(m - c, coins) + countChange(m, r) // sum all the ways with the current coin and all the ways with only the other coins
  }
}
