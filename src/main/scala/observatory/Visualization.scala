package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  import org.apache.commons.math3.util.FastMath._

  val earthRadius = 6271000
  val p = 2

  def distance(l: Location, k: Location): Double = {
    if (l == k) return 0d

    val dLambda = toRadians(abs(l.lon - k.lon))
    val teta1 = toRadians(l.lat)
    val teta2 = toRadians(k.lat)

    earthRadius * acos(sin(teta1) * sin(teta2) + cos(teta1) * cos(teta2) * cos(dLambda))
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    def _weight(from: Location) = {
      val d = distance(from, location)
      if (d <= 1d)
        1d
      else
        1 / pow(d, p)
    }

    val weights =
      (if (temperatures.size > 1000) temperatures.par else temperatures)
      .map({ case (loc, t) => (_weight(loc), t) })

    val exactMatch = weights.find({ case (w, _) => w == 1d })
    if (exactMatch.isDefined)
      return exactMatch.get._2

    val weightedValues = weights.map({ case (locationWeight, temperature) => locationWeight * temperature })
    weightedValues.sum / weights.map(_._1).sum
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    def interpolateValue(c0: (Double, Int), c1: (Double, Int)): Int = {
      val x0: Double = c0._1
      val y0: Double = c0._2
      val x1: Double = c1._1
      val y1: Double = c1._2

      val x = value

      round(
        (y0 * (x1 - x) + y1 * (x - x0))
          /
          (x1 - x0)
      ).toInt
    }

    def _interpolCol(c0: (Double, Color), c1: (Double, Color)): Color = {
      val r = interpolateValue((c0._1, c0._2.red), (c1._1, c1._2.red))
      val g = interpolateValue((c0._1, c0._2.green), (c1._1, c1._2.green))
      val b = interpolateValue((c0._1, c0._2.blue), (c1._1, c1._2.blue))

      Color(r, g, b)
    }

    val previousValues = points.filter { case (v, _) => v <= value }
    val nextValues = points.filter { case (v, _) => v > value }

    //previous closest color
    val c0: Option[(Double, Color)] =
      if (previousValues.isEmpty) None
      else Some(previousValues.minBy { case (v, _) => value - v })

    //next closest color
    val c1: Option[(Double, Color)] =
      if (nextValues.isEmpty) None
      else Some(nextValues.minBy { case (v, _) => v - value })

    (c0, c1) match {
      case (None, Some((_, color))) => color
      case (Some((_, color)), None) => color
      case (None, None) => throw new IllegalArgumentException("No colors provided to interpolate on")
      case (Some(c0Pair), Some(c1Pair)) => _interpolCol(c0Pair, c1Pair)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = generateImage(360, 180, temperatures, colors)

  def renderPixel(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], location: Location): Pixel = {
    val temperature = predictTemperature(temperatures, location)
    val color = interpolateColor(colors, temperature)

    Pixel(color.red, color.green, color.blue, 127)
  }

  def generateImage(w: Int, h: Int, temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    def renderPixel(location: Location): Pixel = {
      Visualization.renderPixel(temperatures, colors, location)
    }

    val rawPixels: Array[Pixel] = new Array(w * h)

    val allCoordinates = for {
      lon <- -180 to 179
      lat <- -89 to 90
    } yield (lon, lat)

    allCoordinates
      .par
      .foreach { case (lon, lat) =>
        val loc = Location(lat, lon)
        val index = locationToIndex(loc)
        rawPixels(index) = renderPixel(loc)
      }

    Image(w, h, rawPixels)
  }

  def locationToIndex(location: Location): Int = {
    var x = location.lon.toInt + 180
    var y = -location.lat.toInt + 90

    x = if (x == 360) 359 else x
    y = if (y == 180) 179 else y

    x + y * 360
  }

}

