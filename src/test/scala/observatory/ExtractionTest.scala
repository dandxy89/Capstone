package observatory

import java.time.LocalDate

import observatory.Extraction._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test(
    "'locationYearlyAverageRecords' should work to find the correct average") {

    // Test Data
    val data: Seq[(LocalDate, Location, Double)] = Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )

    // Expected Results
    val expectedRes: Seq[(Location, Double)] = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )

    assert(locationYearlyAverageRecords(data) === expectedRes)
  }

  test("'fahrenheitToCelsius' should work to find the correct conversion") {

    val expectedRes: Seq[Double] =
      Seq(-2.6111111111111107, -17.77777777777778, -16.666666666666668)

    assert(Seq(27.3, 0.0, 2.0).map(fahrenheitToCelsius) === expectedRes)

  }

  test("'avg' should work to find the correct value") {
    assert(avg(Seq(27.3, 0.0, 2.0)) === 9.766666666666667)
  }

}
