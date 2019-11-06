package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }


    println("Parentheses Balancing")
    val chars1 = List('(', '(', ')', ')', '(')
    val chars2 = List('(', '(', ')', ')')
    println(balance(chars1))
    println(balance(chars2))


    println("Counting Change")
    val coins1 = List(2, 3, 4, 5)
    val coins2 = List(2, 3)
    val money = 10
    println(countChange(money, coins1))
    println(countChange(money, coins2))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c != 0 && c != r) pascal(c - 1, r - 1) + pascal(c, r - 1) else 1
  }

  /**
   * Exercise 2 Parentheses Balancing
   */

  def balance(chars: List[Char]): Boolean = {
    def checkBalance(chars: List[Char], bracketsCounter: Int = 0): Boolean = {
      if (chars.isEmpty) bracketsCounter == 0
      else chars.head match {
        case '(' => checkBalance(chars.tail, bracketsCounter + 1)
        case ')' => bracketsCounter > 0 && checkBalance(chars.tail, bracketsCounter - 1)
        case _ => checkBalance(chars.tail, bracketsCounter)
      }
    }

    checkBalance(chars)
  }

  /**
   * Exercise 3 Counting Change
   * Write a recursive function that counts how many different ways you can make
   * change for an amount, given a list of coin denominations. For example,
   * there is 1 way to give change for 5 if you have coins with denomiation
   * 2 and 3: 2+3.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if ((money < 0) || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
