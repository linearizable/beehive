package io.beehive.actors

import akka.actor._
import akka.persistence._

object UserActor {
    def props(userId: String) = Props(new UserActor(userId))
    
    /**
     * Message when a user interacts with an item
     */ 
    case class UserItemInteraction(userId: String, itemId: String)    
    
    sealed trait UserEvent
    case class ItemAdded(itemId: String) extends UserEvent
}

class UserActor(userId: String) extends PersistentActor with ActorLogging {
    
    import UserActor._
    import ItemActor._
    
    def persistenceId = self.path.name
    
    /**
     * Set of items I have interacted with
     */ 
    val items = scala.collection.mutable.Set[String]()
    
    /**
     * ItemManager actor instance 
     * to send messages to item actors
     */ 
    val itemManager = ItemManager.itemManager
    
    val receiveRecover: Receive = {
        case event: UserEvent => {
            println(event)
            updateState(event)
        }
        case RecoveryCompleted =>  {
            log.info(s"User $userId recovery completed")
        }
    }
    
    /**
     * Update state in response to events
     */ 
    val updateState: UserEvent => Unit = {
        case ItemAdded(itemId: String) => items += itemId
    }
    
    val receiveCommand: Receive = {
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
         
            persist(ItemAdded(itemId))(updateState)
    }
}
