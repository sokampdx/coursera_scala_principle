object session {
  1+2
  def abs(x: Double) = if (x<0) -x else x

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess))
        guess
      else
        sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.0001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  sqrt(9)
  sqrt(100)
  sqrt(2)
  sqrt(0.01)
  sqrt(1e10)

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  gcd(14,21)

  def fact(n: Int): Int =
    if (n == 0) 1 else n * fact(n-1)

  fact(4)

  def fact_tail(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)

    loop(1, n)
  }

  fact_tail(4)

}
