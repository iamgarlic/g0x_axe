package controllers

import play.api.mvc.{Action, Controller}
import play.api.libs.json.Json
import play.api.Routes

import play.api.libs.ws._
import scala.concurrent.Future  
import scala.util.{Success,Failure}
import play.api.libs.concurrent.Execution.Implicits._

case class ParsedResponse(value: String)

object AxeController extends Controller {

  implicit val fooWrites = Json.writes[ParsedResponse]	
  
  val xmlRegEx = """(.*)<table class="table">(.*)</table>.*""".r 
	val tableRegEx = """<tr><td>([^/]*)</td><td>([^/]*)</td><td>([^/]*)</td><td>([^/]*)</td><td>([^/]*)</td><td>([^/]*)</td></tr>""".r 

  def parseLv1 = Action.async {
    val holder /*: WSRequestHolder*/ = WS.url("http://axe-level-1.herokuapp.com/")
    holder.get.map {
      response => 
      	val respBody = response.getAHCResponse.getResponseBody("utf-8").replaceAll("""\n""", "")
				val rows = {
					try{						
						val xmlRegEx(first, table) = respBody						
						for (tableRegEx(name, chinese, math, science, society, health) <- tableRegEx findAllIn table.replaceAll("""\s""", "")) 
						yield {							
						  s"""{"name": "${name}", "grades": {"國語": ${chinese}, "數學": ${math}, "自然": ${science}, "社會": ${society}, "健康教育": ${health}}}"""
						}							
					} catch {
						case x: Throwable => 
							play.Logger.error("Exception:" + x.toString)
							Seq()
					}		
				}
				val js = s"""[${rows.toSeq.drop(1).mkString(",")}]"""
        play.Logger.info("parsed:" + js)
        Ok(Json.toJson(js))
    }     
  }

}