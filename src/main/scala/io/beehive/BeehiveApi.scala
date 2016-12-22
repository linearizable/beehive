package io.beehive

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._

class RestApi(system: ActorSystem, timeout: Timeout) extends RestRoutes {
    implicit val requestTimeout = timeout
    implicit def executionContext = system.dispatcher
}

trait RestRoutes extends BeehiveApi with EventMarshalling {
    import StatusCodes._

    def routes: Route = loadDataRoute ~ recommendationRoute

    def loadDataRoute =
        pathPrefix("loadData") {
            pathEndOrSingleSlash {
                get {
                    onSuccess(loadData) { 
                        complete(OK, "Done")
                }
            }
        }
    }

    def recommendationRoute =
        path("getSimilar" / Segment / Segment) { (itemId, algo) =>
            pathEndOrSingleSlash {
                get {
                    onSuccess(getSimilarResults(itemId, algo)) { similarResults =>
                        complete(OK, similarResults)
                }
            }
        }
    }
}

trait BeehiveApi {
    
    implicit def executionContext: ExecutionContext
    implicit def requestTimeout: Timeout
    
    def loadData() = Future { Beehive.loadData }
    
    def getSimilarResults(itemId: String, algo: String) = Beehive.getSimilarResults(itemId.toInt, algo)
}
