package io.beehive.actors

import akka.actor.Actor
import akka.actor.Props
import io.beehive.lib.Akka

object ItemManager {
    /**
     * Create one ItemManager actor instance which will be used everywhere
     */ 
    val itemManager = Akka.actorSystem.actorOf(Props(classOf[ItemManager]), "ItemManager")
    
    case class GetItemOccurrences(items: List[Int])
    case class ItemOccurrences(occurrences: Map[Int, Int])
}

class ItemManager extends Actor {
    
    import ItemActor._
    import ItemManager._
    
    /**
     * Map to keep track of overall occurrences of the items
     * To be used in similarity calculations
     * Key = ID of the similar item
     * Value = Overall occurrences of the item
     */ 
    var itemOccurrences = scala.collection.mutable.Map[Int, Int]()
    
    def receive = {
        case m @ ItemInteraction(itemId) =>
            println("ItemInteraction")
            
            /**
             * Update item occurrences
             */
            itemOccurrences.get(itemId) match {
                case Some(n) => 
                    itemOccurrences(itemId) += 1
                case None => 
                    itemOccurrences += (itemId -> 1)
            }
            
            /**
             * Forward to item actor
             */   
            itemActor(itemId) forward m
        
        /**
         * Get total occurrenes of all the items in the list
         */ 
        case GetItemOccurrences(items) =>
            val occurrences = items.map(i => (i -> itemOccurrences(i))).toMap
            sender() ! ItemOccurrences(occurrences)
            
        case m @ AddSimilarItem(itemId, _) =>
            //println("AddSimilarItem")
            itemActor(itemId) forward m
            
        case m @ UpdateSimilarItemOccurrences(itemId, _, _) =>
            //println("UpdateSimilarItemOccurrences")
            itemActor(itemId) forward m
            
        case m @ GetCooccurrenceSimilarItems(itemId, _) =>
            itemActor(itemId) forward m
            
        case m @ GetJaccardSimilarItems(itemId, _) =>
            itemActor(itemId) forward m
            
        case m @ GetCosineSimilarItems(itemId, _) =>
            itemActor(itemId) forward m
            
        case m @ GetLLRSimilarItems(itemId, _) =>
            itemActor(itemId) forward m
    }

    /**
     * Get ActorRef for child item actor
     */     
    def itemActor(itemId: Int) = {
        context.child(itemActorName(itemId)).getOrElse {
            context.actorOf(ItemActor.props(itemId), itemActorName(itemId))
        }
    }
    
    /**
     * Get name for the item actor based on item id
     */ 
    def itemActorName(itemId: Int) = "Item:"+itemId.toString()
}
