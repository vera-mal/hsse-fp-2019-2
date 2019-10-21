object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println('\n')
    val s: List[Char] = List(')', '(', ')', ')', ')', '(', '(', '(')
    print(s, balance(s))
    println('\n')
    val s1: List[Char] = List(')', '(', ')', '(', ')', '(', '(', '(')
    print(s1, balance(s1))
    println('\n')
    val arr: List[Int] = List(2, 3, 5)
    print("is " + countChange(18, arr) + " different way's to make " + 18 + " with " + arr + '\n')
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    var i = 1
    if ((c > 0) && (r < 10)) {
      val a = pascal(c - 1, r)
      val b = pascal(c, r + 1)
      i = a + b
    }
    i
  }

  /**
   * Exercise 2 Parentheses Balancing
   */
  def balance(chars: List[Char]): Boolean = {
    var scopeOpen = 0
    var scopeClose = 0
    var result = false
    for (char <- chars)
    {
      if (char == '(')
      {
        scopeOpen += 1
      }
      else if (char == ')')
      {
        scopeClose += 1
      }
    }
    if (scopeOpen == scopeClose)
    {
      result = true
    }
    result
  }

  /**
   * Exercise 3 Counting Change
   * Write a recursive function that counts how many different ways you can make
   * change for an amount, given a list of coin denominations. For example,
   * there is 1 way to give change for 5 if you have coins with denomiation
   * 2 and 3: 2+3.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    var result = 0
    if (money == 0)
    {
      result = 1
    }
    else if (money > 0)
    {
      var i = 0
      while (i < coins.size)
      {
        result += countChange(money - coins(i), coins.slice(i, coins.size))
        i += 1
      }
    }
    result
  }
}
