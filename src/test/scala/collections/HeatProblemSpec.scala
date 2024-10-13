package collections

import org.scalatest.funsuite.AnyFunSuite

class HeatProblemSpec extends AnyFunSuite {
  test("maxHumidityInWindows with basic input") {
    val degrees = List(1, 2, 3, 4)
    assert(HeatProblem.maxHumidityInWindows(degrees, 2) == List(2, 3, 4))
  }

}
