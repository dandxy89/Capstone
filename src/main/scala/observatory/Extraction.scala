package observatory

import java.io.File
import java.time.LocalDate

import scala.collection.parallel.ParSeq
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    *
    * @param xs   Iterable Sequence to find the average
    * @return     Average - sum / count
    */
  def avg(xs: Iterable[Double]): Double = xs.sum / xs.size

  /**
    *
    * @param f    Fahrenheit value
    * @return     Celsius value
    */
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32.0) * (5.0 / 9.0)

  /**
    * @param path   Path of a given file
    * @return       A string of the Absolute Path
    */
  private def filePath(path: String) = {
    new File(this.getClass.getClassLoader.getResource("." + path).toURI).getPath
  }

  /**
    * @param path   Path of a given file
    * @return       A sequence containing Lists
    */
  private def csvReader(path: String): Seq[Seq[String]] = {
    val csvList: Seq[Seq[String]] = Source
      .fromFile(filePath(path))
      .getLines()
      .map(_.split(",").toSeq)
      .filter(_.size > 3)
      .toSeq

    csvList
  }

  /**
    * @param row        Sequence of Data
    * @return           Tuple Mapping the Unique ID to Location
    */
  private def stationApply(row: Seq[String]): ((Any, Any), Location) = {
    getID(row) -> Location(lat = row(2).toDouble, lon = row(3).toDouble)
  }

  /** The WBAN value is not always present, but when present it always uniquely identifies stations.
    * So, in summary, you should always use the WBAN, if available, or fallback to the
    * STN otherwise, to identify stations.
    *
    * @param row        Sequence of Data
    * @return           Tuple of (Any, Any)
    */
  private def getID(row: Seq[String]) = {
    val stn = if (row.head == "") None else row.head
    val wban = if (row(1) == "") None else row(1)
    (stn, wban)
  }

  /**
    * @param row        Sequence of Data
    * @param year       Integer of a Year
    * @param mapping    Location Mapping Unique Identifier to the Location Long Lat values
    * @return           A sequence containing triplets (date, location, temperature)
    */
  private def temperatureApply(
      row: Seq[String],
      year: Int,
      mapping: Map[(Any, Any), Location]): (LocalDate, Location, Double) = {

    (LocalDate.of(year, row(2).toInt, row(3).toInt),
     mapping.getOrElse(getID(row), Location(123123123, 123123123)),
     fahrenheitToCelsius(row(4).toDouble))
  }

  /**
    * @param year               Year number
    * @param stationsFile       Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile   Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return                   A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(
      year: Int,
      stationsFile: String,
      temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    // Read in the Data
    val temperature: Seq[Seq[String]] = csvReader(temperaturesFile)
    val station: Seq[Seq[String]] = csvReader(stationsFile)

    // Apply the Transformations to the Station Data
    val stationParsed: Map[(Any, Any), Location] =
      station.map(stationApply).toMap

    val temperatureParsed: ParSeq[(LocalDate, Location, Double)] =
      temperature.par
        .filter(x => x.size == 5)
        // The temperature field contains a decimal value (or 9999.9 if missing)
        .filter(x => x(4).toDouble != 9999.9)
        // Handling Leap Years...
        .filter(x => !(x(2).toInt == 2 && x(3).toInt == 29 && year % 4 == 0))
        .map(x => temperatureApply(x, year, stationParsed))
        .filter{
          case (_, loc, _) => loc != Location(lat = 123123123, lon = 123123123)
        }

    temperatureParsed.toList
  }

  /**
    * @param records    A sequence containing triplets (date, location, temperature)
    * @return           A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(
      records: Iterable[(LocalDate, Location, Double)])
    : Iterable[(Location, Double)] =
    records
      .groupBy{ case (_, y, _) => y }
      .mapValues(x => avg(x.map{ case (_, _, z) => z.toDouble }))
      .toList
      .sortBy{ case (l, _) => l.lon + l.lat }

}
