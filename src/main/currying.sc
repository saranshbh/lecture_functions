
object currying {

  def mapreduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:Int): Int = {
    if (a>b) 0
    else combine(f(a), mapreduce(f, combine, zero)(a+1, b))
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = mapreduce(f, (x, y) => x * y, 1)(a, b)

  def fact(n: Int): Int = product(x => x)(1, n)


}
