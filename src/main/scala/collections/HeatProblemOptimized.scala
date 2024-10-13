package collections

object HeatProblemOptimized {
  def maxHumidityInWindows(degrees: List[Int], k: Int): List[Int] = {
    @scala.annotation.tailrec
    def maxSlidingWindow(deg: List[Int], deque: List[Int], acc: List[Int]): List[Int] = deg match {
      case Nil => acc.reverse
      case h :: t =>
        val updatedDeque = deque.dropWhile(i => i <= h)
        val newDeque = h :: updatedDeque
        val newAcc = if (newDeque.length >= k) newDeque.head :: acc else acc
        maxSlidingWindow(t, newDeque, newAcc)
    }

    if (degrees.isEmpty || k == 0) List.empty
    else maxSlidingWindow(degrees, List.empty, List.empty)
  }
}
