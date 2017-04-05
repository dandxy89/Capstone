package observatory

import observatory.Extraction.avg
import observatory.Visualization.predictTemperature

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures     Known temperatures
    * @return                 A function that, given a latitude in [-89, 90] and a
    *                         longitude in [-180, 179], returns the predicted temperature at this location
    */
  def makeGrid(
      temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = (y, x) => {
      predictTemperature(temperatures, Location(lat = y, lon = x))
    }

  /**
    * @param temperaturess    Sequence of known temperatures
    * @return                 A function that, given a latitude and a longitude, returns the average
    *                         temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]])
    : (Int, Int) => Double = (y, x) => {
    avg(temperaturess.map(temps => makeGrid(temps)(y, x)))
  }

  /**
    * @param temperatures     Known temperatures
    * @param normals          A grid containing the “normal” temperatures
    * @return                 A sequence of grids containing the deviations compared to the
    *                         normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)],
                normals: (Int, Int) => Double): (Int, Int) => Double = (x, y) => {
    makeGrid(temperatures)(x, y) - normals(x, y)
  }

}
