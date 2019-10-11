package recfun

import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("\nParentheses Balancing ")
    println(balance("()(())()".toCharArray.toList))


    println("\nCounting Change")
    print(countChange(5, List(1, 2, 3)))

  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0 || c > r) {
      throw new IllegalArgumentException("Invalid arguments")
    }

    if (c == 0 || c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  }

  /**
   * Exercise 2 Parentheses Balancing
   */
  def balance(chars: List[Char]): Boolean = {

    def balanceStack(chars: List[Char], stack: ListBuffer[Char]): ListBuffer[Char] = {
      if (chars.isEmpty) {
        return stack
      }

      if (chars.head == '(') {
        stack.addOne('(')
      } else if (chars.head == ')') {
        if (stack.isEmpty) {
          stack.addOne(')')
          return stack
        } else if (stack.last == '(') {
          stack.remove(stack.length - 1)
        }
      }

      balanceStack(chars.tail, stack)

    }

    balanceStack(chars, ListBuffer()).isEmpty

  }

  /**
   * Exercise 3 Counting Change
   * Write a recursive function that counts how many different ways you can make
   * change for an amount, given a list of coin denominations. For example,
   * there is 1 way to give change for 5 if you have coins with denomiation
   * 2 and 3: 2+3.
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (coins.contains(0)) {
      throw new IllegalArgumentException("coins can not contain 0")
    }

    if (money == 0) {
      1
    } else if ((money < 0) || coins.isEmpty) {
      0
    } else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
}
