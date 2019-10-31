//package recfun


object Main {
  def main(args: Array[String]) {

    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println("Check for balance")
    var listWrong1 = List('(', ')', '(')
    var listWrong2 = List(')', '(')
    var listCorrect = List('(', '(', ')', '(', ')', ')')
    println(balance(listWrong1))
    println(balance(listWrong2))
    println(balance(listCorrect))

    println("Count change")
    var coins = List(2, 3)
    println(countChange(5, coins))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ((c < 0) || (c > r)) {0}
    else if (c == 0)  {1}
    else  {pascal(c - 1, r - 1) + pascal(c, r - 1)}
  }

  /**
   * Exercise 2 Parentheses Balancing
   */
  def balance(chars: List[Char]): Boolean = {
    var counter = 0
    def proccessChar(element: Char) = {
      if (element == '(') counter = counter + 1
      else if (element == ')') counter = counter - 1
    }
    for (char <- chars) {
      proccessChar(char)
      if (counter < 0) return false
    }
    if (counter == 0) return true
    else return false
  }

  /**
   * Exercise 3 Counting Change
   * Write a recursive function that counts how many different ways you can make
   * change for an amount, given a list of coin denominations. For example,
   * there is 1 way to give change for 5 if you have coins with denomiation
   * 2 and 3: 2+3.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
    {
      return 1
    }
    if (money < 0 || coins.isEmpty)
    {
      return 0
    }
    return countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}

