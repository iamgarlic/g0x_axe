package services

import scala.concurrent.{Future, Promise}
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._

import play.api.Play.current

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.actor.Actor
import akka.actor.PoisonPill
import akka.actor.Props


object FetchActors { 

  import scala.concurrent.Future
  import play.api.libs.concurrent.Execution.Implicits._
  import play.api.libs.json._
  import models._
  
  case class FetchRequest(val url: String, val headers: Option[Seq[(String, String)]], var response: Option[String])

  class Lv3PageWorker extends Actor {    
    import play.api.libs.ws._
    import akka.pattern.{ ask, pipe }
    def receive = {    
      case (msg: FetchRequest) =>        
        // println(s">>>>>>Start Fetch ! ($msg)" )        
        val holder = msg.headers.isDefined match {
          case true => 
            var rh = WS.url(msg.url)
            msg.headers.get.foreach{h=>              
              rh = rh.withHeaders(h)
            }
            rh
          case false => WS.url(msg.url)
        }
        holder.get.map { response =>
          val ss = response.getAHCResponse.getResponseBody("utf-8").replaceAll("""\n""", "")           
          // println(s">>>>>>Done Fetch ! ($ss)" )
          msg.response = Some(ss)
          msg
        } recover {
          case e: Exception => 
            println(e)
            println(s">>>>>StatWorkker $self fail " + msg.url)
            msg
        } pipeTo sender
    }
  }

  class Lv3PageFetcher extends Actor {        
    import context._
    
    var reqs = scala.collection.mutable.Map[String, Seq[String]]()
    var ps = scala.collection.mutable.Map[String, Promise[Seq[String]]]()
    val tableLv2RegEx = """<tr><td>([^/]*)</td><td>([^/]*)</td><td>([^/]*)</td></tr>""".r  

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

    def receive = {    
      case (msg: FetchRequest, p: Promise[Seq[String]]) =>
        reqs += ((msg.url, Seq()))
        ps += ((msg.url, p))
        val worker = actorOf(Props[Lv3PageWorker])    
        worker ! msg    
      case (msg: FetchRequest) =>
        
        val rows = extractRows(msg.response.get)
        // rows.foreach(row=> println(">>>"+row))
        reqs(msg.url) = reqs(msg.url) ++: rows
        val nextQuery = msg.response.getOrElse("").contains("href='?page=next'")
        if(nextQuery){ 
          println(s">>>>>>Self Next Query" )                    
          actorOf(Props[Lv3PageWorker]) ! msg             
        }  
        else {                  
          ps(msg.url).success(reqs(msg.url))
          reqs -= msg.url
          ps -= msg.url   
        }  

    }
  }

}
