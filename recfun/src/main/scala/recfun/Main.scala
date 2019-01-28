package recfun

import scala.annotation.tailrec

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
  def pascal(c: Int, r: Int): Int = if(c==0 || r==0 || c==r ) 1 else pascal(c-1,r-1) + pascal(c,r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balance_helper(count :Int, myString: List[Char] ):Boolean = myString match {
        case List() => count == 0
        case '('::t   => balance_helper(count+1, t)
        case ')'::t   => if(count == 0) false else balance_helper(count-1, t)
        case _::t => balance_helper(count,t)
      }

      val balance_count = 0
      return balance_helper(balance_count, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(sum: Int, coins: List[Int]): Int = {
      if (sum == 0) return 1

      else if(sum < 0) return 0

      else coins match {
        case Nil => 0
        case x :: remaining_coins => countChange(sum - x, x::remaining_coins) + countChange(sum, remaining_coins)
      }

    }


  }
