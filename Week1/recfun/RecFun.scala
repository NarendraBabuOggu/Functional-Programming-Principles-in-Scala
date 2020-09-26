package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    def checkBalancing(str:String) = {
      val str_lst = str.toList
      val balanced : String = if (balance(str_lst)) "Balanced" else "Not Balanced"
      println(f"The string $str is $balanced")
    }
    println("Checking The Parentheses Balancing")
    checkBalancing(":-)")
    checkBalancing("(if (zero? x) max (/ 1 x))")
    println()
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def checkCharacters(chars:List[Char], stack:List[Char], flag:Boolean) : (List[Char], Boolean) = {
        val (new_stack, new_flag) = {
          if (chars.isEmpty) (stack, flag)
          else if (chars.head == '(') (stack.appended('('), true)
          else if (chars.head == ')' && !stack.isEmpty) (stack.dropRight(1), true)
          else if (chars.head == ')' && stack.isEmpty) (stack, false)
          else (stack, true)
        }

        if (chars.isEmpty) (new_stack, new_flag)
        else checkCharacters(chars.tail, new_stack, new_flag)
      }
      val (new_stack, flag) = checkCharacters(chars, List(), true)
      if (new_stack.isEmpty) flag else false
    }

  /**
  def checkBalance(chars:List[Char], flag:Char) : Char = {
      val new_flag = {
        if (chars.isEmpty) flag
        else if (chars.head == '(' && flag == 'T') 'F'
        else if (chars.head == ')' && flag == 'F') 'T'
        else if (chars.head == '(' || chars.head == ')') 'R'
        else flag
      }
      if (chars.isEmpty) new_flag else {
        if (new_flag == 'R') new_flag else checkBalance(chars.tail, new_flag)
      }
    }
    if (checkBalance(chars, 'T') == 'T') true else false
  }

 */

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(len:Int, total : Int) : Int = {
      if (total == 0) 1
      else if (total <= 0) 0
      else if ((len <= 0) && (total >= 1)) 0
      else count(len-1, total) + count(len, total - coins(len-1))
    }
    if (coins.isEmpty && money <= 0) 1
    else if (coins.isEmpty && money >= 1) 0
    else count(coins.length, money)
  }
}
