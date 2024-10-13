package collections

object HeatProblem {
  def maxHumidityInWindows(degrees: List[Int], k: Int): List[Int] = {
    degrees.sliding(k).map(_.max).toList
  }
}

