package observatory

import observatory.Visualization._

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    val allCoordinates = {
      for {
        lon <- -180 to 179
        lat <- -89 to 90
      } yield ((lat, lon), predictTemperature(temperatures, Location(lat, lon)))
    }.toMap


    (lat: Int, lon: Int) => allCoordinates((lat, lon))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    val grids = temperaturess
      .map(yearlyData => makeGrid(yearlyData))

    val averageGrid = {
      for {
        lon <- -180 to 179
        lat <- -89 to 90
      } yield ((lat, lon), grids.map(g => g(lat, lon)).sum / grids.size)
    }.toMap

    (lat: Int, lon: Int) => averageGrid((lat, lon))
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val deviationGrid = {
      for {
        lon <- -180 to 179
        lat <- -89 to 90
      } yield ((lat, lon), predictTemperature(temperatures, Location(lat, lon)) - normals(lat, lon))
    }.toMap

    (lat: Int, lon: Int) => deviationGrid((lat, lon))
  }
}

