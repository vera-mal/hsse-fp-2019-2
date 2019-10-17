package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Enter line")
    val inString:String = scala.io.StdIn.readLine()
    val input: List[Char] = inString.toList

    if (balance(input)) {
      println("line is balanced")
    } else {
      println("line is not balanced")
    }

    println("Enter coin denominations")
    val inputCoins = scala.io.StdIn.readLine()
    val coins = inputCoins.filter(_!='\n').split(' ').map(_.toInt).toList

    println("Enter amount of money")
    val money = scala.io.StdIn.readInt()

    println ("Amount of possible ways to make a change = " + countChange(money, coins))
  }

  /**
   * Exercise 1
   */

  def factorial (n: Int): Int = {
    if (n > 1)  n * factorial(n - 1)
    else 1
  }

  def pascal(c: Int, r: Int): Int = {
    var elem:Int = 0
    elem = factorial(r) / (factorial(c) * factorial(r-c))
    return elem
  }

  /**
   * Exercise 2 Parentheses Balancing
   */

  def balance(chars: List[Char]): Boolean = {
    var count:Int = 0
    for (i <- 0 until chars.length)
    {
      if (chars(i) == '(') count = count + 1
      if (chars(i) == ')') count = count - 1
      if (count < 0) return false
    }
    if (count == 0) true
    else false
  }

  /**
   * Exercise 3 Counting Change
   * Write a recursive function that counts how many different ways you can make
   * change for an amount, given a list of coin denominations. For example,
   * there is 1 way to give change for 5 if you have coins with denomiation
   * 2 and 3: 2+3.
   * */

  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (_, Nil) => 0
      case (0, _) => 1
      case (money, _) if money < 0 => 0
      case (money, _) => countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }

}