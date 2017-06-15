package observatory

import java.time.LocalDate

import observatory.Extraction._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("testLocateTemperatures") {
    val results = locateTemperatures(2015, "/stations.csv", "/2015.csv")

    assert(results == Seq(
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0),
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0)
    ))
  }

  test("testToCelsius") {
    assert(toCelsius(356d) == 180d)
    assert(toCelsius(212d) == 100d)
    assert(toCelsius(104d) == 40d)
    assert(toCelsius(98.6d) == 37.0d)
    assert(toCelsius(50d) == 10.0)
  }

  test("compute yearly average by location") {
//    [Test Description] [#1 - Data extraction]
//    [Observed Error] an implementation is missing
//    [exception was thrown] detailed error message in debug output section below
//    [Lost Points] 5

    val iterableTemps = locateTemperatures(2015, "/stations.csv", "/2015.csv")
    val result = locationYearlyAverageRecords(iterableTemps).toSeq

    assert(result == Seq(
      (Location(37.358, -78.438), 1.0),
      (Location(37.35, -78.433), 27.3)

    ))
  }

  test("locationYearlyAverageRecords should be able to process 1 million records") {
//    [Test Description][#1 - Data extraction]
//    [Observed Error] an implementation is missing
//    [Lost Points] 5
  }

    test ("weather stations are identified by the composite (STN, WBAN)") {
//      [Test Description][#1 - Data extraction]
//      [Observed Error] Set(
      // (2000 - 01 - 05, Location(5.0, -5.0), 50.0),
      // (2000 - 01 - 04, Location(4.0, -4.0), 50.0),
      // (2000 - 01 - 02, Location(4.0, -4.0), 50.0),
      // (2000 - 01 - 04, Location(2.0, -2.0), 50.0),
      // (2000 - 01 - 01, Location(1.0, -1.0), 50.0),
      // (2000 - 01 - 02, Location(2.0, -2.0), 50.0),
      // (2000 - 01 - 03, Location(3.0, -3.0), 50.0))
      // had size 7 instead of expected size 5
//      [Lost Points] 3
    }

    test ("temperatures are located") {
      val results = locateTemperatures(2000, "/stations.csv", "/2000-one-record.csv")
      assert(results == Seq((LocalDate.of(2000, 1, 29), Location(37.358, -78.438), 10.0d)))
  }
}