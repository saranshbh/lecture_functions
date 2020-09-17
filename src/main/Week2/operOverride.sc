object operOverride{
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x.numer
  y.denom
  x - y - z
  y + y
  x < y
  x maximum y
  new Rational(2)
}

class Rational(x: Int, y: Int){
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if(b==0) a else gcd(b, a % b)

  def numer = x
  def denom = y

  def <(that: Rational) = numer * that.denom < that.numer * denom

  def maximum(that: Rational) = if(this < that) that else this

  def +(that: Rational) =
    new Rational(
      numer * that.denom + denom * that.numer,
      denom * that.denom
    )

  def unary_- : Rational = new Rational(-numer, denom)

  def -(that: Rational) = this + -that


  override def toString = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g}

}