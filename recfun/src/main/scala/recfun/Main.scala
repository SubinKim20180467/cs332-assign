package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c==0)
      1
    else if (r==0)
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def equibrilium(c: List[Char]):Int = {
      if(c.isEmpty)
        0
      else
        if (c.head == '(')
        equibrilium(c.tail) - 1
      else if (c.head == ')')
        equibrilium(c.tail) + 1
      else
        equibrilium(c.tail)
    }
    
    if (equibrilium(chars) == 0)
      true
    else
      false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty)  //exception cases
      0
    else if (money < coins.head)
      0
    else if (money == 0)
      1
    else
    {
      countChange(money-coins.head,coins) + countChange(money,coins.tail)
    }
  }
}
