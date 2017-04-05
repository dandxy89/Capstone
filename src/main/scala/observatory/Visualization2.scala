package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Colors.alpha
import observatory.Interaction.{prepIncrements, tileLocation}
import observatory.Visualization.interpolateColor

import scala.collection.parallel.immutable.ParSeq

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x        X coordinate between 0 and 1
    * @param y        Y coordinate between 0 and 1
    * @param d00      Top-left value
    * @param d01      Bottom-left value
    * @param d10      Top-right value
    * @param d11      Bottom-right value
    * @return         A guess of the value at (x, y) based on the four known values, using
    *                 bilinear interpolation
    *                 See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
      x: Double,
      y: Double,
      d00: Double,
      d01: Double,
      d10: Double,
      d11: Double
  ): Double =
    (1 - y) * ((1 - x) * d00 + x * d10) + y * ((1 - x) * d01 + x * d11)

  /** Method for Computing the Deviation or Normal Grid Images
    *
    * @param grid     Grid to visualize
    * @param colors   Color scale to use
    * @param zoom     Zoom level of the tile to visualize
    * @param x        X value of the tile to visualize
    * @param y        Y value of the tile to visualize
    * @return         The image of the tile at (x, y, zoom) showing the grid using the
    *                 given color scale
    */
  def visualizeGrid(
      grid: (Int, Int) => Double,
      colors: Iterable[(Double, Color)],
      zoom: Int,
      x: Int,
      y: Int
  ): Image = {

    val prepSetup: ParSeq[(Double, Double, Color)] = prepIncrements(x, y, 256.0)
      .par
      .map({ case (i, j) => (i, j, tileLocation(zoom = zoom, x = i, y = j)) })
      .map({
        case (i, j, c) => (i, j, interpolateColor(colors, grid(c.lat.toInt, c.lon.toInt)))
      })

    val data: Array[Pixel] = prepSetup.toList
      .sortBy { case (i, j, _) => (i, j) }
      .map { case (_, _, c) => Pixel(c.red, c.green, c.blue, alpha) }
      .toArray

    Image.apply(w = 256, h = 256, pixels = data)
  }

}
