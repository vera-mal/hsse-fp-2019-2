package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println()
    println("Parentheses Balancing")
    val bracket_list = List('(', '(', ')', ')', ')', '(')
    print("Bracket list is: ")
    for (element <- bracket_list) {
      print(element)
    }
    println()
    print("Is balanced? ")
    println(balance(bracket_list))

    println()
    println("Counting Change")
    print(countChange(50, List(10, 20)))
    println(" way/ways to count change")

  }

  /**
   * Exercise 1
   */

  def pascal(c: Int, r: Int): Int = {
    def factorial(n: Int): Int = {
      if (n < 2) 1 else n * factorial(n - 1)
    }

    factorial(r) / (factorial(c) * factorial(r - c))
  }

  /**
   * Exercise 2 Parentheses Balancing
   */
  def balance(chars: List[Char]): Boolean = {
    @scala.annotation.tailrec
    def isBalanced(chars: List[Char], bracketCounter: Int): Boolean = {
      if (chars.isEmpty) return bracketCounter == 0
      chars.head match {
        case '(' => isBalanced(chars.tail, bracketCounter + 1)
        case ')' => (bracketCounter > 0) && isBalanced(chars.tail, bracketCounter - 1)
        case _ => isBalanced(chars.tail, bracketCounter)
      }
    }

    isBalanced(chars, 0)

  }

  /**
   * Exercise 3 Counting Change
   * Write a recursive function that counts how many different ways you can make
   * change for an amount, given a list of coin denominations. For example,
   * there is 1 way to give change for 5 if you have coins with denomination
   * 2 and 3: 2+3.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if ((money < 0) || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
