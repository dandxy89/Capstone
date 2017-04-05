package observatory

import observatory.Visualization2.bilinearInterpolation
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class Visualization2Test extends FunSuite with Checkers {

  test("'bilinearInterpolation' should work to find the correct value") {
    assert(
      bilinearInterpolation(0.5, 0.2, 91, 162, 210, 95) === 146.10000000000002)
  }

}
