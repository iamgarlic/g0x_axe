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
  val tableLv2RegEx = """<tr><td>([^/]*)</td><td>([^/]*)</td><td>([^/]*)</td></tr>""".r 

  //[<a href="?page=1">1</a>]
  // val pageRegEx = """[<a href="page=1">([0-9]*)</a>]""".r
  val pageRegEx = """<a href="([^/]*)">([0-9]*)</a>""".r

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
        Ok(js).as("application/json")
    }     
  }

  def parseLv2 = Action.async {         
    WS.url("http://axe-level-1.herokuapp.com/lv2").get.flatMap {
      response => 
        val respBody = response.getAHCResponse.getResponseBody("utf-8").replaceAll("""\n""", "")        
        val pages = {
          try{                            
            for (pageRegEx(ps, pn) <- pageRegEx findAllIn respBody) // ps: ?page=1 // pn: 1
            yield{              
              pn
            }             
          } catch {
            case x: Throwable => 
              play.Logger.error("Exception:" + x.toString)
              Seq()
          }   
        }
        
        var results = Map[Int, Seq[String]]()
        var pageFutures = List[Future[Unit]]()     

        pages.foreach{ pn =>           
          val url = s"http://axe-level-1.herokuapp.com/lv2/?page=${pn}"
          println(s"fetch page: $url")
          
          val f = WS.url(url).get.map{ html => 
            val utf8Html = html.getAHCResponse.getResponseBody("utf-8").replaceAll("""\n""", "") 
          
            val rows = { // {"town": "東區", "village": "東勢里", "name" : "林錦全"}
              for (tableLv2RegEx(town, village, name) <- tableLv2RegEx findAllIn utf8Html.replaceAll("""\s""", "")) 
              yield{
                s"""{"town": "${town}", "village": "${village}", "name": "${name}"}"""                
              }
            }
            results += pn.toInt -> rows.drop(1).toSeq //utf8Html
          }  
          pageFutures = f +: pageFutures          
        }

        for(ff <- Future.sequence(pageFutures)) yield {
          // sort by page index, merge to single List, then compose response
          val response = results.toList.sortBy(a=>a._1).map(b=>b._2).flatMap(a=>a).mkString("[",",","]")          
          Ok(response).as("application/json")
        }   
    }  

  }

}