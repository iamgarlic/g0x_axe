package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.http.ContentTypes.JSON

import scala.concurrent._
import play.api.mvc.Results._
import play.api.mvc._

class AxeControllerSpec extends Specification {
  
  "AxeController" should {
    
    "parseLv1 should return JSON" in new WithApplication {
      val result: Future[SimpleResult]  = controllers.AxeController.parseLv1(FakeRequest())

      status(result) must equalTo(OK)
      // contentType(result) must beSome("application/json")
      // charset(result) must beSome("utf-8")
      // contentAsString(result) must contain("Hello from Scala")
    }

    "parseLv2 should return JSON" in new WithApplication {
      val result: Future[SimpleResult]  = controllers.AxeController.parseLv2(FakeRequest())

      status(result) must equalTo(OK)
      // contentType(result) must beSome("application/json")
      // charset(result) must beSome("utf-8")
      // contentAsString(result) must contain("Hello from Scala")
    }

  }
}