package observatory

import java.nio.file.{Files, Paths}
import java.time.{Duration, Instant, LocalDate, LocalDateTime}

import observatory.Extraction._
import observatory.Interaction._

object Main extends App {
  log.info(s"Start generation at ${LocalDateTime.now()}")
  val startTime = Instant.now()

  type Data = Iterable[(LocalDate, Location, Double)]

  val colorScheme = List(
    (60d, Color(255, 255, 255)),
    (32d, Color(255, 0, 0)),
    (12d, Color(255, 255, 0)),
    (0d, Color(0, 255, 255)),
    (-15d, Color(0, 0, 255)),
    (-27d, Color(255, 0, 255)),
    (-50d, Color(33, 0, 107)),
    (-60d, Color(0, 0, 0))
  )

  val yearlyData: Iterable[(Int, Data)] = (2021 to 2021)//(1975 to 2015)
    .map(year => (year, locateTemperatures(year, "/stations_2021.csv", "/" + year + ".csv")))
    .toList


  def generateImage(year:Int, zoom:Int, x:Int, y:Int, temperaturesWithDates:Data): Unit = {
    log.debug(s"Generating tiles for zoom level: $zoom and tile coords: ($x, $y)")

    val temperatures = locationYearlyAverageRecords(temperaturesWithDates)

    Files.createDirectories(Paths.get(s"target/temperatures/$year/$zoom/"))

    val fileName = s"target/temperatures/$year/$zoom/$x-$y.png"
    val tileImage = Interaction.tile(temperatures, colorScheme, zoom, x, y)

    tileImage.output(fileName)
  }

  generateTiles(yearlyData, generateImage)

  log.info(s"Finished generation at ${LocalDateTime.now()}")
  log.info(s"Took ${Duration.between(startTime, Instant.now()).getSeconds} seconds")
}
