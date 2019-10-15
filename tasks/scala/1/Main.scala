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
    var rFact:Int = 1
    for (i <- 1 to r)
      rFact *= i
    var cFact:Int = 1
    for (i <- 1 to c)
      cFact *= i
    var rSubCFact = 1
    for (i <- 1 to r - c)
      rSubCFact *= i
    rFact/(cFact * rSubCFact)
  }

  def balance(chars: List[Char]): Boolean = {
    var count:Int = 0
    var tempList:List[Char] = chars
    for (i <- 1 to chars.length) {
      count += (tempList.head match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      })
      if (count < 0) return false
      tempList = tempList.tail
    }
    if (count != 0) false
    else true
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) return 1
    if ((money < 0) || (coins.isEmpty)) return 0
    countChange(money - coins.head, coins.tail) + countChange(money, coins.tail)
  }
}
