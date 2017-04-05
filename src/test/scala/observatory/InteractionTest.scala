package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  test("'tileLocation' should work to find the correct locations") {

    // Test Data
    val testData: Seq[(Int, Int, Int)] = Seq(
      (0, 0, 0),
      (0, 0, 1),
      (0, 0, 2),
      (0, 0, 3),
      (1, math.pow(2, 0).toInt, 0),
      (2, math.pow(2, 1).toInt, 1),
      (4, math.pow(2, 2).toInt, 2),
      (8, math.pow(2, 3).toInt, 3))

    // Calculated results
    val res = testData
      .map({ case (x, y, z) => observatory.Interaction.tileLocation(zoom = z, x = x, y = y)})
    // X goes from 0 (left edge is 180 °W)    to 2zoom − 1 (right edge is 180 °E)
    // Y goes from 0 (top edge is 85.0511 °N) to 2zoom − 1 (bottom edge is 85.0511 °S) in a Mercator projection
    val expRes = Seq(
      Location(85.05112877980659,-180.0),
      Location(85.05112877980659,-180.0),
      Location(85.05112877980659,-180.0),
      Location(85.05112877980659,-180.0),
      Location(-85.05112877980659,180.0),
      Location(-85.05112877980659,180.0),
      Location(-85.05112877980659,180.0),
      Location(-85.05112877980659,180.0)
    )
    assert(expRes === res)
  }

}
