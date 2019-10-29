object Main {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      (pascal(c - 1, r - 1) + pascal(c, r - 1))
  }

  def balance(chars: List[Char]): Boolean = {

    def subFunction(chars: List[Char], numOpens: Int): Boolean = {

      if (chars.isEmpty) {
        numOpens == 0
      }
      else {

        val head = chars.head
        val number = 0

        if (head == '(') numOpens + 1
        else if (head == '[') numOpens + 1
        else if (head == '{') numOpens + 1
        else if (head == ')') numOpens - 1
        else if (head == ']') numOpens - 1
        else if (head == '}') numOpens - 1
        else numOpens

        if (number >= 0) subFunction(chars.tail, number)
        else false

      }

    }

    subFunction(chars, 0)

  }



  def countChange(money: Int, coins: List[Int]): Int = {

    var amount = 0;

    def subFunction(money: Int, coins: List[Int]): Any = {
      if (!coins.isEmpty) {
        if (money > coins.head) {
          subFunction(money - coins.head, coins)
          subFunction(money, coins.tail)
        }
        else if (money < coins.head) {
          subFunction(money, coins.tail)
        }
        else if (money-coins.head == 0) {
          amount += 1
        }
      }
    }
    subFunction(money, coins.sorted)
    amount
  }
}