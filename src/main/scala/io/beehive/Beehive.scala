package io.beehive

import actors._
import actors.UserActor._
import actors.ItemActor._
import lib.Akka
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.pattern.ask
import akka.util.Timeout

object Beehive {
     
    def loadData() = {
        val userManager = UserManager.userManager
        val itemManager = ItemManager.itemManager
        
        //val lines = scala.io.Source.fromFile("/tmp/s.csv").getLines
        
        //lines.foreach(l => {
            //l.split(",") match {
                //case Array(userId, itemId) =>
                    //userManager ! UserItemInteraction(userId, itemId)
                    //itemManager ! ItemInteraction(itemId)
            //}
        //})
        
        //Thread.sleep(3000)
        //implicit val timeout = Timeout(5 seconds)
        //(1 to 5).foreach(i => {
            //println(i)
            //itemManager.ask(GetCosineSimilarItems(i, 3)).map(println)
            //Thread.sleep(1000)
        //})
        //Thread.sleep(3000)
        
        //Akka.actorSystem.shutdown
    }
    
    def getSimilarResults(itemId: String, algo: String) = {
        implicit val timeout = Timeout(5 seconds)
        algo match {
            case "cooccurrence" => ItemManager.itemManager.ask(GetCooccurrenceSimilarItems(itemId, 3)).mapTo[SimilarResults]
            case "jaccard" => ItemManager.itemManager.ask(GetJaccardSimilarItems(itemId, 3)).mapTo[SimilarResults]
            case "cosine" => ItemManager.itemManager.ask(GetCosineSimilarItems(itemId, 3)).mapTo[SimilarResults]
            case "loglikelihood" => ItemManager.itemManager.ask(GetLLRSimilarItems(itemId, 3)).mapTo[SimilarResults]
        }
    }
}
