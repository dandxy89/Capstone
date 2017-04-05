package observatory

import scala.collection.immutable
import scala.collection.immutable.Seq

/**
  * Contains all the Colours used in the Observatory project
  */
object Colors {

  val tempColors: Seq[(Double, Color)] = Seq(
    (60, Color(red = 255, green = 255, blue = 255)),
    (32, Color(red = 255, green = 0, blue = 0)),
    (12, Color(red = 255, green = 255, blue = 0)),
    (0, Color(red = 0, green = 255, blue = 255)),
    (-15, Color(red = 0, green = 0, blue = 255)),
    (-27, Color(red = 255, green = 0, blue = 255)),
    (-50, Color(red = 33, green = 0, blue = 107)),
    (-60, Color(red = 0, green = 0, blue = 0))
  )

  val devColors: Seq[(Double, Color)] = Seq(
    (7, Color(red = 0, green = 0, blue = 0)),
    (4, Color(red = 255, green = 0, blue = 0)),
    (2, Color(red = 255, green = 255, blue = 0)),
    (0, Color(red = 255, green = 255, blue = 255)),
    (-2, Color(red = 0, green = 255, blue = 255)),
    (-7, Color(red = 0, green = 0, blue = 255))
  )

  /**
    * alpha           Recommend Alpha for the Image
    * generatedSeqs   Generating a Sequence
    * interpolateSeq  Values to approximate using the bilinear interpolation
    */
  val alpha: Int = 127
  val imageDim: Double = 256
  val generatedSeqs: Seq[(Int, Int, Int)] =
    Seq(0, 1, 2, 3).flatMap(x => triples(x))

  /**
    *
    * @param zoom   Zoom level
    * @return       Triple tuples
    */
  def triples(zoom: Int): immutable.IndexedSeq[(Int, Int, Int)] = {
    val upper: Double = math.pow(2, zoom) - 1
    for {
      i <- 0 to upper.toInt
      j <- 0 to upper.toInt
    } yield (zoom, i, j)
  }

}
