package observatory

import observatory.Colors.{devColors, generatedSeqs, tempColors}
import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Manipulation.{average, deviation, makeGrid}
import observatory.Visualization2.visualizeGrid

import scala.collection.immutable

object Main extends App {

  // Week 1: Extraction

  //  val rng: Seq[Int] = Seq(2000) // 1975 to 2015
  //  var rowSum: Int = 0
  //  for (i <- rng ) {
  //    val year: Int = i
  //    val Week1A: Iterable[(LocalDate, Location, Double)] = locateTemperatures(year, "/stations.csv", s"/$year.csv")
  //    val Week1B: Iterable[(Location, Double)] = locationYearlyAverageRecords(Week1A)
  //    rowSum += Week1A.size
  //  }
  //  println(rowSum) // 114,295,260 rows

  // Week 2: Visualization

  lazy val colors: immutable.Seq[(Double, Color)] = tempColors

  //  //  // Iterate through the whole set of years
  //  val rng2: Seq[Int] = 1975 until 2015 //Seq(2015)
  //  for (i <- rng2) {
  //    val year: Int = i
  //    val temperatures1A: Iterable[(LocalDate, Location, Double)] =
  //      locateTemperatures(year, "/stations.csv", s"/$year.csv")
  //    val temperatures1B: Iterable[(Location, Double)] =
  //      locationYearlyAverageRecords(temperatures1A)
  //    val img1: Image = visualize(temperatures1B, colors)
  //    img1.output(new java.io.File(s"target/week2/some-image-$year.png"))
  //  }

  // Week 3: Interactive Visualization

  //  zoom level	tile coverage	              number of tiles	  tile size in degrees
  //  0	          1 tile covers whole world	  1 tile	          360° x 170.1022°
  //  1	          2 × 2 tiles	                4 tiles	          180° x 85.0511°
  //  2	          4 × 4 tiles	                16 tiles	        90° x 42.5256°
  //  n	          2n × 2n tiles	              22n tiles	        360/2n° x 170.1022/2n°

  //  val yearlyData: Stream[(Int, Iterable[(Location, Double)])] = (1975 until 2015)
  //    .map(yr => (yr, locationYearlyAverageRecords(locateTemperatures(yr, "/stations.csv", s"/$yr.csv"))))
  //    .toStream
  //  generateTiles(yearlyData, observatory.Interaction.generateImage)

  // Week 4:

  // 'makeGrid' Example
  //  println("'makeGrid' Example")
  //  (0 until 360 * 180)
  //    .map(x => (x % 360 - 180.0, math.floor(x / 360) - 90))
  //    .foreach({ case(x, y) => println(makeGrid(yearlyData.head)(x.toInt, y.toInt))})

  // 'average' Example
  //  println("'average' Example")
  //  (0 until 360 * 180)
  //    .map(x => (x % 360 - 180.0, math.floor(x / 360) - 90))
  //    .foreach({ case(x, y) => println(average(yearlyData)(x.toInt, y.toInt))})

  // 'deviations' Example
  //  println("'deviations' Example")
  //  (0 until 360 * 180)
  //    .map(x => (x % 360 - 180.0, math.floor(x / 360) - 90))
  //    .foreach({ case(x, y) => println(deviation(temperatures, average(yearlyData))(x.toInt, y.toInt))})

  // Week 5:

  // Bilinear Interpolation - example taken from Wikipedia
  //  println(
  //    (bilinearInterpolation(0.5, 0.2, 91, 162, 210, 95),
  //     bilinearInterpolation(0.5, 0.2, 91, 162, 210, 95) == 146.10000000000002))

  // visualizeGrid
  lazy val yearlyData: Iterable[(Int, Iterable[(Location, Double)])] = (1981 to 2015)
    .map(yr => (yr, locationYearlyAverageRecords(locateTemperatures(yr, "/stations.csv", s"/$yr.csv"))))

  lazy val normalRange: Iterable[(Int, Iterable[(Location, Double)])] = (1975 to 1989)
    .map(yr => (yr, locationYearlyAverageRecords(locateTemperatures(yr, "/stations.csv", s"/$yr.csv"))))

  lazy val deviationRange: Iterable[(Int, Iterable[(Location, Double)])] = (1975 to 2015)
    .map(yr => (yr, locationYearlyAverageRecords(locateTemperatures(yr, "/stations.csv", s"/$yr.csv"))))

  // Yearly Data
  yearlyData.foreach({ case (yr, data) =>
    generatedSeqs.foreach({ case (zoom, x, y) =>
      visualizeGrid(makeGrid(data), tempColors, zoom, x, y)
        .output(new java.io.File(s"target/temperatures/$yr/$zoom/$x-$y.png"))})})

  // Deviations
  deviationRange.foreach({ case (yr, data) =>
    generatedSeqs.foreach({ case (zoom, x, y) =>
      visualizeGrid(deviation(data, average(normalRange.map(_._2))), devColors, zoom, x, y)
        .output(new java.io.File(s"target/deviations/$yr/$zoom/$x-$y.png"))})})

  // Week 6:
  // See the 'interaction2.html' file after running 'capstoneUI/fastOptJS' in sbt.

}
