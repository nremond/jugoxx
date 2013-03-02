package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._


class MinesweeperSpec extends Specification {
  
  "Minesweeper" should {
    
    "resolve the first problem" in {
      running(FakeApplication()) {
val input=
"""4 4
*...
....
.*..
....
"""        

        val resolve = route(FakeRequest(POST, "/minesweeper/resolve", FakeHeaders(), input)).get
        
        status(resolve) must equalTo(OK)

val solution = 
"""*100
2210
1*10
1110"""
        contentAsString(resolve) must equalTo(solution)
      }
    }

    "resolve the second problem" in {
      running(FakeApplication()) {
val input=
"""3 5
**...
.....
.*..."""        

        val resolve = route(FakeRequest(POST, "/minesweeper/resolve", FakeHeaders(), input)).get
        
        status(resolve) must equalTo(OK)

val solution = 
"""**100
33200
1*100"""
        contentAsString(resolve) must equalTo(solution)
      }
    }


  }

}