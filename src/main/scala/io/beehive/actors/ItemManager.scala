package io.beehive.actors

import akka.actor._
import akka.persistence._
import io.beehive.lib.Akka

object ItemManager {
    /**
     * Create one ItemManager actor instance which will be used everywhere
     */ 
    val itemManager = Akka.actorSystem.actorOf(Props(classOf[ItemManager]), "ItemManager")
    
    case class GetItemOccurrences(items: List[String])
    case class ItemOccurrences(occurrences: Map[String, Int])
    
    sealed trait ItemManagerEvent
    case class ItemAdded(itemId: String) extends ItemManagerEvent
}

class ItemManager extends PersistentActor with ActorLogging {
    
    import ItemActor._
    import ItemManager._
    
    def persistenceId = self.path.name
    
    /**
     * Map to keep track of overall occurrences of the items
     * To be used in similarity calculations
     * Key = ID of the similar item
     * Value = Overall occurrences of the item
     */ 
    var itemOccurrences = scala.collection.mutable.Map[String, Int]()
    
    val receiveRecover: Receive = {
        case event: ItemManagerEvent => {
            println(event)
            updateState(event)
        }
        case RecoveryCompleted =>  {
            log.info("ItemManager recovery completed")
            /**
             * Recover all child item actors
             */ 
            itemOccurrences.foreach(i => itemActor(i._1))
        }
    }
    
    /**
     * Update state in response to events
     */ 
    val updateState: ItemManagerEvent => Unit = {
        case ItemAdded(itemId: String) => 
            /**
             * Update item occurrences
             */
            itemOccurrences.get(itemId) match {
                case Some(n) => 
                    itemOccurrences(itemId) += 1
                case None => 
                    itemOccurrences += (itemId -> 1)
            }
    }
    
    val receiveCommand: Receive = {
        case m @ ItemInteraction(itemId) =>
            
            /**
             * Forward to item actor
             */   
            itemActor(itemId) forward m
            
            persist(ItemAdded(itemId))(updateState)
        
        /**
         * Get total occurrenes of all the items in the list
         */ 
        case GetItemOccurrences(items) =>
            val occurrences = items.map(i => (i -> itemOccurrences(i))).toMap
            sender() ! ItemOccurrences(occurrences)
            
        case m @ AddSimilarItem(itemId, _) =>
            //println("AddSimilarItem")
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
    def itemActor(itemId: String) = {
        context.child(itemActorName(itemId)).getOrElse {
            context.actorOf(ItemActor.props(itemId), itemActorName(itemId))
        }
    }
    
    /**
     * Get name for the item actor based on item id
     */ 
    def itemActorName(itemId: String) = "Item:"+itemId
}
