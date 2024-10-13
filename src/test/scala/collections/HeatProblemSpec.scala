package collections

import org.scalatest.funsuite.AnyFunSuite

class HeatProblemSpec extends AnyFunSuite {

  test("maxHumidityInWindows with basic input") {
    val degrees = List(1, 2, 3, 4)
    assert(HeatProblem.maxHumidityInWindows(degrees, 2) == List(2, 3, 4))
  }

  test("maxHumidityInWindows with empty list") {
    val degrees = List.empty[Int]
    assert(HeatProblem.maxHumidityInWindows(degrees, 2) == List.empty)
  }

  test("maxHumidityInWindows with window size greater than list size") {
    val degrees = List(1, 2, 3)
    assert(HeatProblem.maxHumidityInWindows(degrees, 5) == List(3))
  }

  test("maxHumidityInWindows with window size 1") {
    val degrees = List(1, 2, 3, 4)
    assert(HeatProblem.maxHumidityInWindows(degrees, 1) == List(1, 2, 3, 4))
  }

  test("maxHumidityInWindows with repeated values") {
    val degrees = List(3, 3, 3, 3)
    assert(HeatProblem.maxHumidityInWindows(degrees, 2) == List(3, 3, 3))
  }

  test("maxHumidityInWindows with negative values") {
    val degrees = List(-5, -2, -3, -1, -4)
    assert(HeatProblem.maxHumidityInWindows(degrees, 3) == List(-2, -1, -1))
  }

  test("maxHumidityInWindows with window size 0") {
    val degrees = List(1, 2, 3, 4)
    assert(HeatProblem.maxHumidityInWindows(degrees, 0) == List.empty)
  }

  test("maxHumidityInWindows with sorted ascending values") {
    val degrees = List(1, 2, 3, 4, 5)
    assert(HeatProblem.maxHumidityInWindows(degrees, 2) == List(2, 3, 4, 5))
  }

  test("maxHumidityInWindows (optimized) with basic input") {
    val degrees = List(1, 2, 3, 4)
    assert(HeatProblemOptimized.maxHumidityInWindows(degrees, 2) == List(2, 3, 4))
  }

  test("maxHumidityInWindows (optimized) with empty list") {
    val degrees = List.empty[Int]
    assert(HeatProblemOptimized.maxHumidityInWindows(degrees, 2) == List.empty)
  }

  test("maxHumidityInWindows (optimized) with window size greater than list size") {
    val degrees = List(1, 2, 3)
    assert(HeatProblemOptimized.maxHumidityInWindows(degrees, 5) == List(3))
  }

  test("maxHumidityInWindows (optimized) with window size 1") {
    val degrees = List(1, 2, 3, 4)
    assert(HeatProblemOptimized.maxHumidityInWindows(degrees, 1) == List(1, 2, 3, 4))
  }

  test("maxHumidityInWindows (optimized) with repeated values") {
    val degrees = List(3, 3, 3, 3)
    assert(HeatProblemOptimized.maxHumidityInWindows(degrees, 2) == List(3, 3, 3))
  }

  test("maxHumidityInWindows (optimized) with negative values") {
    val degrees = List(-5, -2, -3, -1, -4)
    assert(HeatProblemOptimized.maxHumidityInWindows(degrees, 3) == List(-2, -1, -1))
  }

  test("maxHumidityInWindows (optimized) with window size 0") {
    val degrees = List(1, 2, 3, 4)
    assert(HeatProblemOptimized.maxHumidityInWindows(degrees, 0) == List.empty)
  }

  test("maxHumidityInWindows (optimized) with sorted ascending values") {
    val degrees = List(1, 2, 3, 4, 5)
    assert(HeatProblemOptimized.maxHumidityInWindows(degrees, 2) == List(2, 3, 4, 5))
  }

}
