package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._


class DietSpec extends Specification {
  
  "Diet" should {
    
    "resolve the first problem" in {
      running(FakeApplication()) {
        val json = Json.arr(
            Json.obj("name" -> "coca-light", "value" -> 1),
            Json.obj("name" -> "croissant", "value" -> 180),
            Json.obj("name" -> "au-travail-a-velo", "value" -> -113),
            Json.obj("name" -> "guitar-hero", "value" -> -181)
          )

        val request = FakeRequest("POST", "/diet/resolve").withJsonBody(json)
        val resolve = route(request).get
        
        //println("recu="+contentAsString(resolve))
        status(resolve) must equalTo(OK)

        val content = contentAsString(resolve)
        content must contain("coca")
        content must contain("croissant")
        content must contain("guitar")
      }
    }


  }

}
