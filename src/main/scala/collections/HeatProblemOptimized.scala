package collections

object HeatProblemOptimized {
  def maxHumidityInWindows(degrees: List[Int], k: Int): List[Int] = {
    if (degrees.isEmpty) List.empty
    else if (k <= 0) List.empty
    else if (k >= degrees.length) List(degrees.max)
    else {
      val deque = scala.collection.mutable.ArrayDeque[Int]()
      val result = scala.collection.mutable.ListBuffer[Int]()

      for (i <- degrees.indices) {
        if (deque.nonEmpty && deque.head <= i - k) deque.removeHead()

        while (deque.nonEmpty && degrees(deque.last) <= degrees(i)) deque.removeLast()

        deque.append(i)

        if (i >= k - 1) result.append(degrees(deque.head))
      }

      result.toList
    }
  }
}
