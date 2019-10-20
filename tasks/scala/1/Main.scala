//package recfun
//import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    val chars1:List[Char] = List('(', 'a', 'd', 'd', ')', 'l', ')')
    val chars2:List[Char] = List('(', '(', 'a', 'd', 'd', ')', 'l', ')')
    val chars3:List[Char] = List(')', 'a', 'd', 'd', '(', 'l')
    val chars4:List[Char] = List('(', ')', 'a', 'd', 'd', ')', 'l', '(')
    println(balance(chars1))
    println(balance(chars2))
    println(balance(chars3))
    println(balance(chars4))
    val coins:List[Int] = List(1, 2, 3, 1)
    println(countChange(5, coins))
    println(countChange(3, coins))
  }

  def pascal(c: Int, r: Int): Int = {
    def factorial(x: Int): Int = {
      if (x != 0) return x * factorial(x - 1) else {
        return 1
      }
    }
    factorial(r)/(factorial(c) * factorial(r - c))
  }

  def balance(chars: List[Char]): Boolean = {
    def countOfParentheses(list: List[Char], count: Int, isBalanced: Boolean): (Boolean, Int) = {
      if (count < 0) return (false, count)
      if (list.isEmpty) {
        return (isBalanced, count)
      }
      list.head match {
        case '(' => countOfParentheses(list.tail, count + 1, isBalanced)
        case ')' => countOfParentheses(list.tail, count - 1, isBalanced)
        case _ => countOfParentheses(list.tail, count, isBalanced)
      }
    }
    if (countOfParentheses(chars, 0, true) == (true, 0)) {
      return true
    }
    else false
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) return 1
    if ((money < 0) || (coins.isEmpty)) return 0
    countChange(money - coins.head, coins.tail) + countChange(money, coins.tail)
  }
}
