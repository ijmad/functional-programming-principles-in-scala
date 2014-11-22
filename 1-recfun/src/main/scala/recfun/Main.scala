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
   *
   * Compute the elements of a Pascal's triangle.
   * Numbers at the edge of the triangle are all 1, numbers inside are the sum of the two above it.
   *
   * Example:
   *
   *      1
   *     1 1
   *    1 2 1
   *   1 3 3 1
   *  1 4 6 4 1
   *
   * pascal(0,2) = 1, pascal(1,2) = 2 and pascal(1,3) = 3
   *
   */
  def pascal(c: Int, r: Int): Int = c match {
    case 0 | `r` =>
      1
    case _ =>
      pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   *
   * A recursive function which verifies the balancing of parentheses in a string, which we
   * represent as a List[Char] not a String.
   *
   * For example, the function should return true for the following strings:
   *   (if (zero? x) max (/ 1 x))
   *   I told him (that it’s not (yet) done). (But he wasn’t listening)
   *   The function should return false for the following strings:
   * But false for:
   *   :-)
   *   ())(
   */
  def balance(chars: List[Char], open: Int = 0): Boolean = {
      if (chars.size == 0)
        open == 0
      else
        chars.head match {
          case '(' =>
            balance(chars.tail, open + 1)
          case ')' =>
            if (open > 0)
              balance(chars.tail, open - 1)
            else
              false
          case _ =>
            balance(chars.tail, open)
        }
  }

  /**
   * Exercise 3
   *
   * A recursive function that counts how many different ways you can make change for an amount, given a list of coin
   * denominations. For example, there are 3 ways to give change for 4 if you have coins with denomination 1 and 2:
   *   1+1+1+1, 1+1+2, 2+2
   */
  def countChange(money: Int, coins: List[Int]): Int = money match {
    case 0 => // money handled
      1
    case _ if coins.isEmpty => // no more coins
      0
    case _ =>
      if (coins.head > money) countChange(money, coins.tail) // can't use coin
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
