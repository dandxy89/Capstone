package observatory

import scala.collection.immutable.Seq

/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 {

  /**
    * @return                 The available layers of the application
    */
  def availableLayers: Seq[Layer] = {

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

    Seq(
      Layer(LayerName.Temperatures, tempColors, 1975 to 2015),
      Layer(LayerName.Deviations, devColors, 1990 to 2015)
    )
  }

  /**
    * @param selectedLayer    A signal carrying the layer selected by the user
    * @return                 A signal containing the year bounds corresponding to the
    *                         selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] = {
    selectedLayer().layerName.id match {
      case a if a == "temperatures" => Signal(1975 to 2015)
      case b if b == "deviations" => Signal(1990 to 2015)
    }
  }

  /**
    * @param selectedLayer    The selected layer
    * @param sliderValue      The value of the year slider
    * @return                 The value of the selected year, so that it never goes out of the layer bounds.
    *                         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *                         this method should return the closest value that is included
    *                         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer],
                    sliderValue: Signal[Int]): Signal[Int] = {
    Signal(sliderValue() match {
      case a if a >= selectedLayer().bounds.max => selectedLayer().bounds.max
      case b if b <= selectedLayer().bounds.min => selectedLayer().bounds.min
      case _ => sliderValue() + 1
    })
  }

  /**
    * @param selectedLayer    The selected layer
    * @param selectedYear     The selected year
    * @return                 The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer],
                      selectedYear: Signal[Int]): Signal[String] = {
    Signal(s"src/main/resources/${selectedLayer().layerName.id}/${selectedYear()}/{z}/{x}/{y}.png")
  }

  /**
    * @param selectedLayer    The selected layer
    * @param selectedYear     The selected year
    * @return                 The caption to show
    */
  def caption(selectedLayer: Signal[Layer],
              selectedYear: Signal[Int]): Signal[String] = {
    Signal(s"${selectedLayer().layerName.id.capitalize} (${selectedYear()})")
  }

}

sealed abstract class LayerName(val id: String)
object LayerName {
  case object Temperatures extends LayerName("temperatures")
  case object Deviations extends LayerName("deviations")
}

/**
  * @param layerName          Name of the layer
  * @param colorScale         Color scale used by the layer
  * @param bounds             Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName,
                 colorScale: Seq[(Double, Color)],
                 bounds: Range)
