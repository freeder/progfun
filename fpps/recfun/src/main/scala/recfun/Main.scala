package recfun
import common._

object Main {

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    //Transform triangle to this picture
    //1
    //1 1
    //1 2 1
    //1 3 3 1
    //.....
    //With that in main the edge cases are
    //First row element is 1
    if (r == 0) 1
    //First column always 1
    else if (c == 0) 1
    //The slope is always 1
    else if (c == r) 1
    //Else we are inside so usual pascal calculus
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    balanceIter(0, chars)
  }

  def balanceIter(counter: Int, chars: List[Char]): Boolean = {
    if (chars.isEmpty) (counter == 0)
    else if (counter < 0) false
    else {
      val next:Char = chars.head
      //consume remaining chars
      if (next == '(') balanceIter(counter + 1, chars.tail)
      else if (next == ')') balanceIter(counter - 1, chars.tail)
      else balanceIter(counter, chars.tail)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 0
    else if (coins.length == 0) 0
    else {
      val remaining:Int = (money - coins.head)
      (if (remaining == 0) 1 else if (remaining > 0) countChange(remaining, coins) else 0) + countChange(money, coins.tail)
    }
  }
}
