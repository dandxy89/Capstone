package observatory

import observatory.Colors.tempColors
import observatory.Visualization._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.math._

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("'haversineDistance' should work to find the correct conversion") {

    val data = Seq(
      (Location(40.750307, -73.994819), Location(40.749641, -73.99527)),
      (Location(47.64828, -122.52963), Location(47.61168, -122.33326))
    )

    val res =
      data.map({ case (x, y) => greatCircleDistance(x, y) })
    assert(Seq(0.05171811091255072, 9.486853744381255) === res)

  }

  test("Beyond the 'max' of the interpolation of colours") {

    val colors: Iterable[(Double, Color)] = tempColors
    assert(
      Color(red = 255, green = 255, blue = 255) === interpolateColor(colors,
        100.0))

  }

  test("Beyond the 'min' of the interpolation of colours") {
    val colors: Iterable[(Double, Color)] = tempColors
    assert(
      Color(red = 0, green = 0, blue = 0) === interpolateColor(colors, -100.0))
  }

  test("Interpolation of colours") {
    val colors: Iterable[(Double, Color)] = tempColors
    assert(Color(33, 0, 107) === interpolateColor(colors, -49.99999999999))
  }

  test("Grader Test for Interpolation of colours") {
    val colors: Iterable[(Double, Color)] =
      Seq((-103.0, Color(255, 0, 0)), (1.0, Color(0, 0, 255)))
    assert(Color(128, 0, 128) === interpolateColor(colors, -51.0))
  }

  test("Grader Test 2 for Interpolation of colours") {
    val colors: Iterable[(Double, Color)] =
      Seq((-1.0, Color(255, 0, 0)), (0.0, Color(0, 0, 255)))
    assert(Color(191, 0, 64) === interpolateColor(colors, -0.75))
  }

  test("scale = List((0.0,Color(255,0,0)), (1.0,Color(0,0,255))), value = 0.5") {
    val scale = List((0.0, Color(255, 0, 0)), (1.0, Color(0, 0, 255)))
    assert(interpolateColor(scale, 0.5) == Color(128, 0, 128))
  }

  test("color rbg should restrict to 0 to 255") {
    val scale: Seq[(Double, Color)] = tempColors
    for {
      i <- -200 to 200
    } {
      val color = interpolateColor(scale, i)
      assert(color.red <= 255 && color.red >= 0)
      assert(color.green <= 255 && color.green >= 0)
      assert(color.blue <= 255 && color.blue >= 0)
    }
  }

  test("same location should have distance 0") {
    val hongKongUniversity = Location(22.282980, 114.137072)
    val guangzhouUniversity = Location(23.038006, 113.368560)
    val taipeiCity = Location(25.032054, 121.562533)
    val moscow = Location(55.747926, 37.680536)
    val locations =
      List(hongKongUniversity, guangzhouUniversity, taipeiCity, moscow)
    assert(locations.map(i => greatCircleDistance(i, i)).sum === 0)

  }

  test(
    "predicted temperature at location z should be closer to known temperature at location x than to " +
      "known temperature at location y, if z is closer (in distance) to x than y, and vice versa") {
    val guangzhouTower = Location(23.105518, 113.325589)
    val hongKongUniversity = Location(22.282980, 114.137072)
    val guangzhouUniversity = Location(23.038006, 113.368560)
    val taipeiCity = Location(25.032054, 121.562533)
    val moscow = Location(55.747926, 37.680536)
    val celsius = List(28D, 25D, 22D, 6D)
    val locations =
      List(hongKongUniversity, guangzhouUniversity, taipeiCity, moscow)
    assert(
      predictTemperature(locations.zip(celsius), guangzhouTower) > 22D && predictTemperature(
        locations.zip(celsius),
        guangzhouTower) < 28D)
  }

  test("color distance") {

    implicit class ColorDistance(color: Color) {
      def distance(other: Color): Double = {
        val dr = pow(other.red - color.red, 2)
        val dg = pow(other.green - color.green, 2)
        val db = pow(other.blue - color.blue, 2)
        sqrt(dr + dg + db)
      }
    }

    val scale: Seq[(Double, Color)] = tempColors
    val sortedScales = scale.sortBy({ case (p, _) => p })
    for {
      index <- 0 until (sortedScales.length - 1)
    } {
      val s1 = sortedScales(index)
      val s2 = sortedScales(index + 1)

      for {
        i <- s1._1.toInt until s2._1.toInt
      } {
        val color = interpolateColor(scale, i)

        val mid = (s1._1 + (s2._1 - s1._1) / 2).toInt

        i match {
          case a if i == mid => assert(true)
          case b if i < mid =>
            assert(color.distance(s1._2) < color.distance(s2._2),
              s"($s1,$s2) temperature: $i->$color")
          case _ =>
            assert(color.distance(s1._2) > color.distance(s2._2),
              s"($s1,$s2) temperature: $i->$color")
        }
      }
    }
  }

}
