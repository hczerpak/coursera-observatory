package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import Visualization._
import com.sksamuel.scrimage.Pixel

import scala.util.Try

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  val red = Color(255, 0, 0)
  val green = Color(0, 255, 0)
  val blue = Color(0, 0, 255)

  test("interpolateColor should return lowest color for value below the scale") {
    val colorScale = List((-10d, blue), (10d, red))
    val result = interpolateColor(colorScale, -20)

    assert(result == blue)
  }

  test("interpolateColor should return highest color for value above the scale") {
    val colorScale = List((-10d, blue), (10d, red))
    val result = interpolateColor(colorScale, 110)

    assert(result == red)
  }

  test("interpolateColor should return N if r/g/b value is equal to N in both colors") {
    val colorScale = List((-10d, Color(0, 255, 255)), (10d, Color(0, 255, 0)))
    val result = interpolateColor(colorScale, 1)

    assert(result.green == 255)
  }

  test("Me thinks interpolateColor should throw an exception if iterable is empty") {
    assert(Try.apply({
      interpolateColor(List(), 0)
    }).isFailure)
  }

  test("interpolateColor should interpolate correct color") {
    val points = List((0d, red), (10d, green), (20d, blue))
    val result = interpolateColor(points, 5d)

    assert(result.blue == 0)
    assert(result.red == 128)
    assert(result.green == 128)
  }

  test("locationToIndex calculates correct indexes in the array") {
    assert(locationToIndex(Location(90, -180)) == 0)
    assert(locationToIndex(Location(90, -179)) == 1)
    assert(locationToIndex(Location(89, -180)) == 360)
    assert(locationToIndex(Location(-89, 179)) == 360 * 180 - 1)
  }

  test("all pixels have values") {
    val colorScale = List((-10d, blue), (10d, red))
    val measurements = List(
      (Location(0, 0), 10d),
      (Location(90, -180), -10d),
      (Location(90, 180), -10d),
      (Location(-90, 180), -10d),
      (Location(-90, -180), -10d)
    )

    val img = visualize(measurements, colorScale)
    img.output(new java.io.File("target/some-image.png"))

    assert(!img.pixels.contains(null))
  }

  test("pixels with samples at exact locations should have exact colors from color scale") {
    val colorScale = List((-10d, blue), (10d, red))
    val measurements = List(
      (Location(0, 0), 10d),
      (Location(90, -180), -10d),
      (Location(90, 180), -10d),
      (Location(-90, 180), -10d),
      (Location(-90, -180), -10d)
    )

    val img = visualize(measurements, colorScale)

    //blue corners
    assert(img.pixel(0, 0) == Pixel(0, 0, 255, 127))
    assert(img.pixel(359, 0) == Pixel(0, 0, 255, 127))
    assert(img.pixel(359, 179) == Pixel(0, 0, 255, 127))
    assert(img.pixel(0, 179) == Pixel(0, 0, 255, 127))

    //red middle
    assert(img.pixel(180, 90) == Pixel(255, 0, 0, 127))
  }

  test("distance() should always return a value") {
    val d = distance(Location(1,1), Location(90, -180))

    assert(!Double.NaN.equals(d))
  }
}
