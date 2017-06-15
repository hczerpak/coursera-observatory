package observatory

import com.sksamuel.scrimage.Image
import org.apache.commons.math3.util.FastMath._
import org.slf4j.{Logger, LoggerFactory}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val lon = x.toDouble / (1 << zoom) * 360.0 - 180.0
    val lat = toDegrees(atan(sinh(PI * (1.0 - 2.0 * y.toDouble / (1 << zoom)))))

    Location(
      f"$lat%.4f".toDouble,
      f"$lon%.4f".toDouble
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zoom`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val pixelLocations =
      for {
        yi <- 0 until 256
        xi <- 0 until 256
      } yield tileLocation(zoom + 8, 256 * x + xi, 256 * y + yi)

    val pixels = pixelLocations
      .map(l => Visualization.renderPixel(temperatures, colors, l))
      .toArray

    Image(256, 256, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Int, Data)],
                           generateImage: (Int, Int, Int, Int, Data) => Unit
                         ): Unit = {

    (if (yearlyData.size > 1000) yearlyData else yearlyData.par)
      .par
      .foreach { case (year, data) =>
        for {
          zoom <- 0 to 3
          x <- 0 until (1 << zoom)
          y <- 0 until (1 << zoom)
        } yield generateImage(year, zoom, x, y, data)
      }
  }
}
