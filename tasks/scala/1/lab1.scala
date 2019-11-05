package recfun
//import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + "\t")
      println()
    }

    println("\nParentheses Balancing")
    print("(() ")
    println(balance(List[Char]('(','(',')')))
    print("(()) ")
    println(balance(List[Char]('(','(',')',')')))
    print("(())) ")
    println(balance(List[Char]('(','(',')',')',')')))


    println("\nCounting Change")
    println(countChange(8, List[Int](1, 4, 8)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == r)
	  1
    else if (c == 0)
	  1
    else
	  pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2 Parentheses Balancing
   */
  def balance(chars: List[Char]): Boolean = {
    var amount = 0
    var isOkey = true
    for (i <- chars)
      {
        if (i == '(')
          {
            amount += 1
          }
        else if (i == ')')
          {
            amount -= 1
          }
        if (amount < 0)
          {
            isOkey = false
          }
      }
    if (amount > 0)
      {
        isOkey = false
      }

    isOkey
  }

  /**
   * Exercise 3 Counting Change
   * Write a recursive function that counts how many different ways you can make
   * change for an amount, given a list of coin denominations. For example,
   * there is 1 way to give change for 5 if you have coins with denomiation
   * 2 and 3: 2+3.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty )
	  0
    else if (money == 0 )
	  1
    else 
	  countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}