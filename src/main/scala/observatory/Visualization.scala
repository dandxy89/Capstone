package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Colors.alpha

import scala.collection.parallel.immutable.ParSeq

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    *
    * @param xs             Iterable of temperatures
    * @return               Average value
    */
  def avgDual(xs: Iterable[(Location, Double)]): Double =
    xs.map { case (_, b) => b }.sum / xs.size

  /**
    *
    * @param location1      Location Class values
    * @param location2      Location Class values
    * @return               Using the Haversine Distance calculate the distance value in km
    */
  def greatCircleDistance(location1: Location, location2: Location): Double = {
    val deltaLat = math.toRadians(location2.lat - location1.lat)
    val deltaLong = math.toRadians(location2.lon - location1.lon)
    val a = math.pow(math.sin(deltaLat / 2), 2) + math.cos(math.toRadians(
      location1.lat)) * math.cos(math.toRadians(location2.lat)) * math.pow(
      math.sin(deltaLong / 2),
      2)

    // Great Circle Distance
    3958.761 * 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
  }

  /**
    * @param temperatures   Known temperatures: pairs containing a location and
    *                       the temperature at this location
    * @param location       Location where to predict the temperature
    * @return               The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)],
                         location: Location): Double = {

    val within: Iterable[(Location, Double)] = temperatures
      .filter({ case (l, _) => l == location })

    if (within.size == 1) {
      within.head._2

    } else {
      // Using the so-called "Inverse Distance Weighting" method or IDW, the weight of any known point
      // is set inversely proportional to its distance from the estimated point.
      val distances: Iterable[(Double, Double)] = temperatures
      // weights, calculation
        .map({
          case (loc, tmp) =>
            (1 / math.pow(greatCircleDistance(loc, location), 4), tmp)
        })
        // Filter out Infinity and zero
        .filter({ case (wgt, _) => !wgt.isInfinity && !wgt.isNaN && wgt != 0 })

      val wu: Double = distances
        .map({ case (wgt, tmp) => wgt * tmp })
        .filter(x => !x.isInfinity && !x.isNaN)
        .sum

      wu / distances.map({ case (f, _) => f }).sum match {
        case x if x.isNaN => avgDual(temperatures)
        case x => x
      }
    }
  }

  /**
    *
    * @param value          The value to interpolate
    * @param lowerCol       Lower Color value
    * @param upperCol       Upper Color value
    * @param lower          Lower Temperature
    * @param upper          Upper Temperature
    * @return               Interpolated value
    */
  def interpolate1D(value: Double,
                    lowerCol: Double,
                    upperCol: Double,
                    lower: Double,
                    upper: Double): Int = {
    val calc = upperCol - (upper - value) * (upperCol - lowerCol) / (upper - lower)

    // Round to 3 decimal places, round and then finally then to Int
    math.round((calc * 1000).round / 1000.toDouble).toInt
  }

  /**
    * @param points         Pairs containing a value and its associated color
    * @param value          The value to interpolate
    * @return               The color that corresponds to `value`, according to the color
    *                       scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)],
                       value: Double): Color = {

    val pointsSorted: Seq[(Double, Color)] = points.toSeq
      .sortWith { _._1 > _._1 }

    value match {
      case _ if value >= points.maxBy { case (k, _) => k }._1 =>
        points.maxBy { case (p, _) => p }._2
      case _ if value <= points.minBy { case (a, _) => a }._1 =>
        points.minBy { case (a, _) => a }._2
      case _ if pointsSorted.exists(point => point._1 == value) =>
        pointsSorted
          .dropWhile(point => point._1 != value)
          .head
          ._2
      case _ =>
        val lower: (Double, Color) =
          pointsSorted.filter { case (v, _) => v <= value }.head
        val upper: (Double, Color) =
          pointsSorted.filter { case (v, _) => v > value }.last

        val (lowerTemp: Double, lowerColor: Color) = lower
        val (upperTemp: Double, upperColor: Color) = upper
        Color(
          red = interpolate1D(value = value,
                              lower = lowerTemp,
                              upper = upperTemp,
                              lowerCol = lowerColor.red,
                              upperCol = upperColor.red),
          green = interpolate1D(value = value,
                                lower = lowerTemp,
                                upper = upperTemp,
                                lowerCol = lowerColor.green,
                                upperCol = upperColor.green),
          blue = interpolate1D(value = value,
                               lower = lowerTemp,
                               upper = upperTemp,
                               lowerCol = lowerColor.blue,
                               upperCol = upperColor.blue)
        )
    }
  }

  /**
    * @param temperatures   Known temperatures
    * @param colors         Color scale
    * @return               A 360×180 image where each pixel shows the predicted
    *                       temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)],
                colors: Iterable[(Double, Color)]): Image = {

    val setup: ParSeq[(Double, Double, Pixel)] = (0 until 360 * 180).par
      .map(x => (x % 360 - 180.0, math.floor(x / 360) - 90))
      .map { case (x, y) => (x, y, Location(lat = y, lon = x)) }
      .map {
        case (x, y, c) =>
          (x, y, interpolateColor(colors, predictTemperature(temperatures, c)))
      }
      .map { case (x, y, c) => (x, y, Pixel(c.red, c.green, c.blue, alpha)) }

    val data: Array[Pixel] = setup.toList
      .sortBy { case (x, y, _) => (-y, x) }
      .map { case (_, _, calc) => calc }
      .toArray

    Image.apply(w = 360, h = 180, pixels = data)
  }
}
