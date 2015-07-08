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
    else {
      var next:Char = chars.head
      //I do not like this code, other ways??
      var incr:Int = 0
      if (next == '(') incr = 1
      else if (next == ')') incr = -1
      //consume remaining chars
      if (counter < 0) false
      else balanceIter(counter + incr, chars.tail)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    var options:List[List[Int]] = countChangeIter(money, coins)
    //println("Changes for (" + money + ") with coins(" + coins + " are " + options)
    options.length
  }

  def countChangeIter(money: Int, coins: List[Int]): List[List[Int]] = {
    if (money == 0) List()
    else if (!Option(coins).isDefined) List()
    else if (coins.length == 0) List()
    else {
      var changes:List[List[Int]] = List()
      // Try with first coin
      var coin:Int = coins.head
      var remaining:Int = (money - coin)
      if (remaining == 0) {
        //If no change remaining we have a change
        changes = changes :+ List(coin)
      } else if (remaining > 0) {
        //Else we continue counting with remaining
        countChangeIter(remaining, coins).map((list) => changes = changes :+ (coin::list))
      }

      //Once first coin is exhausted then search only with other coins
      countChangeIter(money, coins.tail).map((list) => changes = changes :+ list)

      changes
    }
  }
}
