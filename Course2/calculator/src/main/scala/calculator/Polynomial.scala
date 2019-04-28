package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {

    Signal{
      val b_val = b()
      (b_val*b_val)- (4*a()*c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Signal{
      val del_val = delta()
      if(del_val < 0)
      {
        Set()
      }
      else
      {
        val a_val = a()
        val b_val = b()
        Set( (-1*b_val + scala.math.sqrt(del_val) )/2 * a_val,  (-1*b_val - scala.math.sqrt(del_val) )/2 * a_val)
      }

    }
  }
}
