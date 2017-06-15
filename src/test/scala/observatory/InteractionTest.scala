package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import Interaction._

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  test("No idea how to test for extra tiles") {
    //  [Test Description] [#3 - I?nteractive visualization] generateTiles covers all the expected tiles
    //    [Observed Error] noExtraTiles was false Unexpected tiles: Set((1984,(3,4,8)), (1903,(3,1,8)), (1997,(3,8,8)), (1903,(3,0,8)), (1984,(3,8,3)), (1986,(2,4,4)), (1940,(0,0,1)), (1986,(3,3,8)), (1920,(3,8,0)), (1962,(2,4,1)))
    //  [Lost Points] 3

  }

  test("tileLocation must return the corresponding latitude and longitude, given some Web Mercator coordinates") {
    //    [Test Description] [#3 - Interactive visualization] tileLocation must return the corresponding latitude and longitude, given some Web Mercator coordinates
    //    [Observed Error] -180.0000 did not equal 85.0511
    //    [Lost Points] 2

    assert(tileLocation(0, 0, 0) == Location(85.0511, -180))
  }
}
