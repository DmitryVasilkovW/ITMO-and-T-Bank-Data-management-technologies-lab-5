package collections

object HeatProblem {
  def maxHumidityInWindows(degrees: List[Int], k: Int): List[Int] = {
    if (k <= 0) List.empty
    else degrees.sliding(k).map(_.max).toList
  }
}
