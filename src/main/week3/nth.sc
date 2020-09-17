import week3._

object nth{
  def nth[T](n: Int, xs: List[T]): T = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if(n==0) xs.head
    else nth(n-1, xs.tail)
  }

  val list = new cons(1, new cons(2, new cons(3, new Nil)))

  nth(2, list)
  nth(-1, list)
}