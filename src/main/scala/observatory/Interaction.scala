package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Colors.{alpha, generatedSeqs, imageDim, tempColors}
import observatory.Visualization.{interpolateColor, predictTemperature}

import scala.collection.immutable.Seq
import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    *
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return
    */
  def prepIncrements(x: Int, y: Int, imageDim: Double): Seq[(Double, Double)] = {
    val increments: Seq[(Double, Double)] = for {
      i <- 0 until imageDim.toInt
      j <- 0 until imageDim.toInt
    } yield {
      (if (i == 0) x else x + i / imageDim,
        if (j == 0) y else y + j / imageDim)
    }
    increments
  }

  /**
    * @param zoom   Zoom level
    * @param x      X coordinate
    * @param y      Y coordinate
    * @return       The latitude and longitude of the top-left corner of the tile, as
    *               per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Double, y: Double): Location = {
    val lonDeg: Double = x.toDouble / (1 << zoom) * 360.0 - 180.0
    val latDeg: Double = toDegrees(
      atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1 << zoom)))))
    Location(lat = latDeg, lon = lonDeg)
  }

  /**
    * @param temperatures   Known temperatures
    * @param colors         Color scale
    * @param zoom           Zoom level
    * @param x              X coordinate
    * @param y              Y coordinate
    * @return               A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)],
           colors: Iterable[(Double, Color)],
           zoom: Int,
           x: Int,
           y: Int): Image = {

    // Calculate the Data
    val setup = prepIncrements(x, y, imageDim).par
      .map{ case (n, m) => tileLocation(zoom = zoom, x = n, y = m) }
      .map{ case (c) => (c.lon, c.lat,
        interpolateColor(colors, predictTemperature(temperatures, c))) }
      .map{ case (a, b, c) => (a, b, Pixel(c.red, c.green, c.blue, alpha)) }

    val data: Array[Pixel] = setup.toList
      .sortBy{ case (f, g, _) => (-g, f) }
      .map{ case (_, _, calc) => calc }
      .toArray

    Image.apply(w = imageDim.toInt, h = imageDim.toInt, pixels = data)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData     Sequence of (year, data), where `data` is some data associated with
    *                       `year`. The type of `data` can be anything.
    * @param generateImage  Function that generates an image given a year, a zoom level, the x and
    *                       y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
      yearlyData: Iterable[(Int, Data)],
      generateImage: (Int, Int, Int, Int, Data) => Unit): Unit = {

    yearlyData
      .foreach{
        case (year, temps) =>
          generatedSeqs
            .foreach({
              case (zoom, x, y) => generateImage(year, zoom, x, y, temps)
            })
      }
  }

  /** User Defined function for generating the images
    *
    * @param year         Year number
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @param inputs       Known temperatures
    */
  def generateImage(year: Int,
                    zoom: Int,
                    x: Int,
                    y: Int,
                    inputs: Iterable[(Location, Double)]): Unit = {

    // Requirement: 'target/temperatures/<year>/<zoom>/<x>-<y>.png'
    tile(
      temperatures = inputs,
      colors = tempColors,
      zoom = zoom,
      x = x,
      y = y
    ).output(new java.io.File(s"target/temperatures/$year/$zoom/$x-$y.png"))
  }

}
