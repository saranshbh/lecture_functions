import math.abs

object fixedpoint{

  val tolerance = 0.00001
  def isCloseEnough(x: Double, y: Double) =
    abs((x-y)/x) / x < tolerance

  def fixPoint(f: Double => Double)(firstGuess: Double) = {

    def iterate(guess: Double): Double= {
      val next = f(guess)
      println("guess = " + guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)

  }
  fixPoint(x=> 1 + x/2)(1)

  def sqrt(x: Double) = fixPoint(y => (y + x/y)/2)(1)

}