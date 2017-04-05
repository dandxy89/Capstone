package observatory

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class GridSquare(d00: Double, d01: Double, d10: Double, d11: Double)
