package io.beehive.actors

import akka.actor.Actor
import akka.actor.Props

object UserActor {
    def props(userId: Int) = Props(new UserActor(userId))
    
    /**
     * Message when a user interacts with an item
     */ 
    case class UserItemInteraction(userId: Int, itemId: Int)    
}

class UserActor(userId:Int) extends Actor {
    
    import UserActor._
    import ItemActor._
    
    /**
     * Set of items I have interacted with
     */ 
    val items = scala.collection.mutable.Set[Int]()
    
    /**
     * ItemManager actor instance 
     * to send messages to item actors
     */ 
    val itemManager = ItemManager.itemManager
    
    def receive = {
        /**
         * I interacted with an item
         */ 
        case UserItemInteraction(_, itemId) =>
        
            /**
             * Send AddSimilarItem message to all the items 
             * user has previously interacted with
             */
            items.foreach(i => {
                itemManager ! AddSimilarItem(i, itemId)
                itemManager ! AddSimilarItem(itemId, i)
            })  
         
            /**
             * Add item to items
             */ 
            items += itemId
    }
}
