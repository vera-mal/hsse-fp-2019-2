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
    if ((c == 0) || (c == r)) {
      1
    } else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
   * Exercise 2 Parentheses Balancing
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], balance: Int): Boolean = {
      if (chars.isEmpty) {
        balance == 0
      } else if (chars.head == '(') {
        loop(chars.tail, balance + 1)
      } else if (chars.head == ')') {
        (balance > 0) && loop(chars.tail, balance - 1)
      } else {
        loop(chars.tail, balance)
      }
    }

    loop(chars, 0)
  }


  /**
   * Exercise 3 Counting Change
   * Write a recursive function that counts how many different ways you can make
   * change for an amount, given a list of coin denominations. For example,
   * there is 1 way to give change for 5 if you have coins with denomiation
   * 2 and 3: 2+3.
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (_, Nil)             => 0
    case (tmp, _) if tmp < 0  => 0
    case (0, _)               => 1
    case (tmp, _)             => countChange(tmp, coins.tail) + countChange(tmp - coins.head, coins)
  }
}
