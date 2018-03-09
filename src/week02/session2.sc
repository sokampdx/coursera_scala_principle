object session2 {
  def sum(f: Int => Int, a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop (a + 1, f(a) + acc)

    loop(a, 0)
  }

  sum(x => x, 3, 5)
  sum(x => x * x, 3, 5)

  def product(f: Int => Int) (a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  product( x => x * x) (1, 2)
  product( x => x * x) (2, 3)
  product( x => x * x) (2, 4)
  product( x => x * x) (3, 4)

  def fact(n: Int) = product(x => x) (1, n)

  fact(5)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
  }

  def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x,y) => x * y, 1)(a, b)
  def fact2(n: Int) = product2(x => x)(1, n)

  product2(x => x * x)(3,4)
  fact2(5)

}