object recfun {
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c, r-1) + pascal(c-1, r-1)
  }

  pascal(0,0)
  pascal(0,1)
  pascal(0,10)
  pascal(10,10)
  pascal(1,2)
  pascal(1,3)
  pascal(2,3)
  pascal(2,4)

  def balance(chars: List[Char]): Boolean = {
    def level_check(chars: List[Char], total: Int): Boolean = {
      if (total < 0 || (chars.isEmpty && total != 0)) false
      else if (chars.isEmpty && total == 0) true
      else level_check(chars.tail, score(chars.head, total))
    }

    def score(c: Char, total: Int): Int = {
        if ( c == '(' ) total + 1
        else if ( c == ')' ) total - 1
        else total
    }

    level_check(chars, 0)
  }

  balance("(".toList)
  balance("(if (zero? x) max (/ 1 x))".toList)
  balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)
  balance(":-)".toList)
  balance("())(".toList)

  def countChange(money: Int, coins: List[Int]): Int = {
    def count(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0) 0
      else if (coins.isEmpty && money >=1) 0
      else count(money, coins.tail) + count(money - coins.head, coins)
    }
    count(money, coins.sortWith(_.compareTo(_) < 0))
  }

  countChange(4, List(1,2))
  countChange(10, List(5))
  countChange(2, List(3))
  countChange(5, List(1,2,5))
  countChange(6, List(1,2,5))
}