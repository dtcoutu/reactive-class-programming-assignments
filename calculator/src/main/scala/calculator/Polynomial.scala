package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(Math.pow(b(), 2) - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    //(-b ± √Δ) / (2a)
    Signal {
      if (delta() < 0) {
        Set.empty
      } else {
        val first = (-b() + Math.sqrt(delta())) / (2 * a())
        val second = (-b() - Math.sqrt(delta())) / (2 * a())
        if (first == second) {
          Set(first)
        } else {
          Set(first, second)
        }
      }
    }
  }
}
