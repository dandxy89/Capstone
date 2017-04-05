package observatory

import observatory.Manipulation.{average, makeGrid}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class ManipulationTest extends FunSuite with Checkers {

  test("'makeGrid' test from grader") {

    val testData: Seq[(Location, Double)] = Seq(
      (Location(lat = -45.0, lon = 0.0), 30.0),
      (Location(lat = 45.0, lon = -90.0), 5.0)
    )

    assert(makeGrid(testData)(90, -153) < 15)
    assert(makeGrid(testData)(45, -90) === 5.0)
  }

  test("'average' test from grader") {

    val testData2: Seq[Seq[(Location, Double)]] = Seq(
      Seq(
        (Location(lat = -46.0, lon = 1.0), 30.0),
        (Location(lat = 46.0, lon = -89.0), 5.0)
      ),
      Seq(
        (Location(lat = -46.0, lon = 1.0), 15.0),
        (Location(lat = 46.0, lon = -89.0), 10.0)
      )
    )

    assert(average(testData2)(90, -153) === 7.66256013240788)
    assert(average(testData2)(45, -90) === 7.500000154858218)
  }

}
