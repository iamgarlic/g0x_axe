package controllers

import play.api.mvc.{Action, Controller}
import play.api.libs.json.Json
import play.api.Routes

import play.api.libs.ws._
import scala.concurrent.{Future, Promise}
import scala.util.{Success,Failure}
import play.api.libs.concurrent.Execution.Implicits._

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.actor.Actor
// import akka.actor.PoisonPill
import akka.actor.Props


case class ParsedResponse(value: String)

object AxeController extends Controller {

  implicit val fooWrites = Json.writes[ParsedResponse]  
  
  // val xmlRegEx = """(.*)<table class="table">(.*)</table>.*""".r 
  val tableRegEx = """<tr><td>([^/]*)</td><td>([^/]*)</td><td>([^/]*)</td><td>([^/]*)</td><td>([^/]*)</td><td>([^/]*)</td></tr>""".r 
  val tableLv2RegEx = """<tr><td>([^/]*)</td><td>([^/]*)</td><td>([^/]*)</td></tr>""".r 

  val pageRegEx = """<a href="([^/]*)">([0-9]*)</a>""".r


  val system = ActorSystem("mySystem")

  def parseLv1 = Action.async {
    WS.url("http://axe-level-1.herokuapp.com/").get.map {
      response => 
        val respBody = response.getAHCResponse.getResponseBody("utf-8").replaceAll("""\n""", "")
        val rows = {
          try{            
            // val xmlRegEx(first, table) = respBody           
            for (tableRegEx(name, chinese, math, science, society, health) <- tableRegEx findAllIn respBody.replaceAll("""\s""", "")) 
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
        
        // var results = Map[Int, String]()
        var pageFutures = List[Future[(Int, String)]]()     

        pages.foreach{ pn =>           
          val url = s"http://axe-level-1.herokuapp.com/lv2/?page=${pn}"
          // println(s"fetch page: $url")
          
          val f = WS.url(url).get.map{ html => 
            val utf8Html = html.getAHCResponse.getResponseBody("utf-8").replaceAll("""\n""", "") 
            pn.toInt -> utf8Html          
          }  
          pageFutures = f +: pageFutures          
        }

        for(ff <- Future.sequence(pageFutures)) yield {
          val parsed = ff.map{ result =>
            result._1 -> extractRows(result._2)            
          }

          // println(s"done merging: ${parsed.size}")
          // sort by page index, merge to single List, then compose response
          val response = parsed.toList.sortBy(a=>a._1).map(b=>b._2).flatMap(a=>a).mkString("[",",","]")          
          Ok(response).as("application/json")
        }   
    }  

  }

  // 
  def parseLv3 = Action.async {         
    WS.url("http://axe-level-1.herokuapp.com/lv3").get.flatMap {
      response => 
        val respBody = response.getAHCResponse.getResponseBody("utf-8").replaceAll("""\n""", "")           
        val cookieRegEx = """(.*); path=(.*)""".r
        // println(s"Set-Cookie: $session")
        val header = response.header("Set-Cookie") match {
          case Some(s) => 
            val cookieRegEx(cookie, path) = s            
            Seq("Cookie" -> cookie.toString)
          case _ =>               
            Seq()
        }

        val next = "http://axe-level-1.herokuapp.com/lv3/?page=next"
        
        var results = extractRows(respBody)
        var rows = Seq[String]()
        for(aggrigated <- fetch(next, header, rows)) 
        yield {
          results = results ++: aggrigated          
          val js = s"""[${results.mkString(",")}]"""
          Ok(js).as("application/json")
        }
    }  
  }

  def fetch(url: String, headers: Seq[(String, String)], results: Seq[String]): Future[Seq[String]] = {
    var holder = WS.url(url)
    headers.foreach{ h => 
      holder = holder.withHeaders(h)
    }

    holder.get.flatMap{ response =>
      val ss = response.getAHCResponse.getResponseBody("utf-8").replaceAll("""\n""", "")                 
      val newResults = results ++: extractRows(ss)

      // next?
      if(ss.contains("href='?page=next'"))
        fetch(url, headers, newResults)
      else
        Future{newResults}  
    }
  }

  def extractRows(input: String) : Seq[String] = {
    var rows = Seq[String]()
    rows = { // {"town": "東區", "village": "東勢里", "name" : "林錦全"}
      for (tableLv2RegEx(town, village, name) <- tableLv2RegEx findAllIn input.replaceAll("""\s""", "")) 
      yield{
        s"""{"town": "${town}", "village": "${village}", "name": "${name}"}"""                
      }
    }.drop(1).toSeq  
    rows
  }

  def parseLv4 = Action.async {  

    val userAgent = """Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/34.0.1847.137 Safari/537.36"""       
    WS.url("http://axe-level-4.herokuapp.com/lv4").withHeaders("User-Agent"->userAgent).get.flatMap {
      response => 
        val respBody = response.getAHCResponse.getResponseBody("utf-8").replaceAll("""\n""", "")        
        // println(s">>> $respBody")
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

        var pageFutures = List[Future[(Int, String)]]()     

        pages.foreach{ pn =>           
          val url = s"http://axe-level-4.herokuapp.com/lv4/?page=${pn}"
          val ref = "http://axe-level-4.herokuapp.com/lv4/?page=%d".format(pn.toInt-1)
          // println(s"fetch page: $url referer: $ref")

          val holder = pn match {
            case "1" => WS.url(url).withHeaders("User-Agent" -> userAgent)
            case _ => WS.url(url).withHeaders("Referer"->ref, "User-Agent" -> userAgent)
          }
          
          val f = holder.get.map{ html => 
            val utf8Html = html.getAHCResponse.getResponseBody("utf-8").replaceAll("""\n""", "")                         
            pn.toInt -> utf8Html
          }  
          pageFutures = f +: pageFutures          
        }

        for(ff <- Future.sequence(pageFutures)) yield {          
          val parsed = ff.map{ result =>
            result._1 -> extractRows(result._2)            
          }
          // println(s"done merging: ${parsed.size}")
          // sort by page index, merge to single List, then compose response
          val response = parsed.toList.sortBy(a=>a._1).map(b=>b._2).flatMap(c=>c).mkString("[",",","]")          
          Ok(response).as("application/json")
        }   
    }  

  }

}