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
    def nextRow (vals: Vector[Int]) = 
       (0 +: vals, vals :+ 0).zipped.map(_ + _)
      
    def row(r:Int): Vector[Int] =
      if (r == 0)
        Vector(1)
      else
        nextRow(row(r-1))
        
    row(r)(c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIncrement(c:Char): Int = 
      if (c == '(') 
        1 
      else if (c == ')') 
        -1
      else
        0
      
    def balance(count:Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty)
        count == 0
      else if (count < 0) 
        false
      else 
        balance(count + balanceIncrement(chars.head), chars.tail)
    }
  
    balance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count (money: Int, coins: List[Int]): Int =
      if (money == 0)
        1
      else if (coins.isEmpty)
        0
      else {
        val coin = coins.head
        val maxOfCoin = money / coin
        val partials =
          for (howMany <- maxOfCoin to 0 by -1) 
            yield count(money-howMany*coin, coins.tail)
        partials.reduce(_ + _)
      }
        
    count(money,coins.sorted.reverse)
  }
}
