package observatory

import java.time.LocalDate
import java.util.Objects._
import java.util.concurrent.ConcurrentHashMap

import org.apache.commons.math3.util.FastMath._
import org.apache.spark.{SparkConf, SparkContext}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  @transient private lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("observatory")
  @transient private lazy val sc: SparkContext = new SparkContext(conf)

  private case class Station(name: String, lat: Double, lon: Double) extends Serializable {
    def location: Location = Location(lat, lon)
  }

  private def key(part1: String, part2: String) = {
    part1 + "++" + part2
  }

  /** Load postings from the given file */
  private def toStation(arr: Array[String]): Station = {
    Station(
      name = key(arr(0), arr(1)),
      lat = arr(2).toDouble,
      lon = arr(3).toDouble
    )
  }

  /** (°F − 32) x 5/9 = °C */
  def toCelsius(farenheight: Double): Double = {
    val rawConversion = (farenheight - 32) * 5 / 9d
    round(rawConversion * 10) / 10d
  }

  private case class Temperature(name: String, localDate: LocalDate, value: Double) extends Serializable

  private def toTemperature(arr: Array[String], year: Int): Temperature = {
    Temperature(
      name = key(arr(0), arr(1)),
      localDate = LocalDate.of(year, arr(2).toInt, arr(3).toInt),
      value = toCelsius(arr(4).toDouble)
    )
  }

  private def loadLines(fileName: String) = scala.io.Source
    .fromInputStream(getClass.getResourceAsStream(fileName))
    .getLines

  private def validLine(line: Array[String]): Boolean = {
    line.length == 5 &&
      line(0).length + line(1).length > 0
  }

  private def validStationLine(line: Array[String]): Boolean = {
    line.length == 4 &&
      line(0).length + line(1).length > 0 &&
      line(2).length > 0 &&
      line(3).length > 0
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    requireNonNull(stationsFile)
    requireNonNull(temperaturesFile)

    //start from stations by key
    val stationsMap: ConcurrentHashMap[String, Extraction.Station] = new ConcurrentHashMap[String, Extraction.Station]()
    loadLines(stationsFile)
      .map(line => line.split(","))
      .filter(validStationLine)
      .map(toStation)
      .foreach(s => stationsMap.put(s.name, s))

    //temperatures by station key
    val temperatures = loadLines(temperaturesFile)
      .map(line => line.split(","))
      .filter(validLine)
      .map(line => toTemperature(line, year))
      .map(t => (t.name, t))

    join(temperatures, stationsMap)
  }

  def join(temperatures: Iterator[(String, Extraction.Temperature)],
           stationsMap: ConcurrentHashMap[String, Extraction.Station]): Iterable[(LocalDate, Location, Double)] = {
    temperatures
      .filter({ case (name, _) => stationsMap.containsKey(name) })
      .map({ case (name, t) => (t.localDate, stationsMap.get(name).location, t.value) })
      .toList
      .sortBy(_._1.toEpochDay)
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records
      .groupBy(_._2)
      .mapValues(iterable => iterable.aggregate(0d)({ case (last, trio) => trio._3 + last }, _ + _) / iterable.size)
  }
}
