package io.beehive.actors

import akka.actor.Actor
import akka.actor.Props

import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.collection.immutable.ListMap

import io.beehive.lib._

object ItemActor {
    def props(itemId: Int) = Props(new ItemActor(itemId))
    
    /**
     * Message when a user interacts with an item
     */ 
    case class ItemInteraction(itemId: Int) 
    
    /** 
     * Add an item to similar items list
     */ 
    case class AddSimilarItem(itemToAddTo: Int, itemToBeAdded: Int)
    
    /**
     * Message to update total occurrences of a similar item
     */ 
    case class UpdateSimilarItemOccurrences(targetItem: Int, itemToBeUpdated: Int, occurrences: Int)
    
    /**
     * Messages to fetch similar items using different similairty metrics
     */ 
    case class GetCooccurrenceSimilarItems(itemId: Int, numItems: Int)
    case class GetJaccardSimilarItems(itemId: Int, numItems: Int)
    case class GetCosineSimilarItems(itemId: Int, numItems: Int)
    case class GetLLRSimilarItems(itemId: Int, numItems: Int)
    
    /**
     * Output of the similarity calculation
     * Key: Similar Item Id
     * Value: Similarity score (based on similarity metric chosen)
     */ 
    case class SimilarResult(itemId: Int, score: Double) 
    case class SimilarResults(results: List[SimilarResult])
    
    object SimilarResult {
        /**
         * Construnctors for SimilarResult which takes a tuple
         */ 
        //def apply(t: (Int, Int)) : SimilarResult = SimilarResult(t._1, t._2.toDouble)
        def apply(t: (Int, Double)) : SimilarResult = SimilarResult(t._1, t._2)
    }
}

class ItemActor(itemId:Int) extends Actor {
    
    import ItemActor._
    import UserManager._
    import ItemManager._
    
    var occurrences = 0
    /**
     * List of items similar to me
     * Key = ID of the similar item
     * Value = No. of occurrences together
     */ 
    var similarItems = scala.collection.mutable.Map[Int, Int]()

    /**
     * UserManager actor to send user related messages
     */ 
    val userManager = UserManager.userManager
    
    def receive = {
        
        /**
         * A user interacted with me
         */ 
        case ItemInteraction(_) => 
            /**
             * Increment my total occurrences
             */
            occurrences += 1     
    
        /**
         * Add or update itemToBeAdded in my similarItems list
         */     
        case AddSimilarItem(_, itemToBeAdded) =>
            
            similarItems.get(itemToBeAdded) match {
                case Some(n) => 
                    /**
                     *  Item already exists in the list, increment occurrences together
                     */ 
                    similarItems(itemToBeAdded) += 1
                case None =>
                    /**
                     * Item does not already exist in the list.
                     * Add item and set 1 as occurrences together
                     */ 
                    similarItems += (itemToBeAdded -> 1)
            }
        
        /**
         * Get similar items using cooccurrence similarity
         */     
        case GetCooccurrenceSimilarItems(_, numItems) =>
            val cooccurrence = new Cooccurrence(similarItems.toMap, numItems)
            sender() ! SimilarResults(cooccurrence.similarity)
        
        /**
         * Get similar items using jaccard similarity
         */         
        case GetJaccardSimilarItems(_, numItems) =>
            val s = sender()             
            getSimilarItemOccurrences().map(io => {
                val jaccard = new Jaccard(similarItems.toMap, occurrences, io.occurrences, numItems)
                s ! SimilarResults(jaccard.similarity)
            })
        
        /**
         * Get similar items using cosine similarity
         */     
        case GetCosineSimilarItems(_, numItems) =>
            val s = sender()             
            getSimilarItemOccurrences().map(io => {
                val cosine = new Cosine(similarItems.toMap, occurrences, io.occurrences, numItems)
                s ! SimilarResults(cosine.similarity)
            })
        
        /**
         * Get similar items using log likelihood similarity
         */     
        case GetLLRSimilarItems(_, numItems) =>
            /**
             * For log likelihood ratio, we need the total number of users
             * which we need to ask the UserManager
             */
            val s = sender()             
            
            val numUsers = getNumUsers()
            val itemOccurrences = getSimilarItemOccurrences()
            
            for {
                nu <- numUsers
                io <- itemOccurrences
            } yield {
                val loglikelihood = new LogLikelihood(similarItems.toMap, occurrences, io.occurrences, nu.num, numItems)
                s ! SimilarResults(loglikelihood.similarity)
            }
    }
    
    /**
     * Fetch total occurrences of all similar items from ItemManager
     */
    def getSimilarItemOccurrences() : Future[ItemOccurrences] = {
        implicit val timeout = Timeout(5 seconds)
        val similarItemList = similarItems.map(_._1).toList
        context.parent.ask(GetItemOccurrences(similarItemList)).mapTo[ItemOccurrences]
    }
    
    /**
     * Fetch total number of users seen in the system
     */ 
    def getNumUsers() : Future[NumUsers] = {
        implicit val timeout = Timeout(5 seconds)
        userManager.ask(GetNumUsers).mapTo[NumUsers]
    }
}
