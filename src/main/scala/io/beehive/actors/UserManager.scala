package io.beehive.actors

import akka.actor._
import akka.persistence._
import akka.actor.Props
import io.beehive.lib.Akka

object UserManager {
    /**
     * Create one UserManager actor instance which will be used everywhere
     */ 
    val userManager = Akka.actorSystem.actorOf(Props(classOf[UserManager]), "UserManager")
    
    case object GetNumUsers
    case class NumUsers(num: Int)
    
    sealed trait UserManagerEvent
    case class UserAdded(userId: String) extends UserManagerEvent
}

class UserManager extends PersistentActor with ActorLogging {
    
    import UserActor._
    import UserManager._
    
    def persistenceId = self.path.name
    
    /**
     * Set of users
     */ 
    val users = scala.collection.mutable.Set[String]()
    
    val receiveRecover: Receive = {
        case event: UserManagerEvent => {
            println(event)
            updateState(event)
        }
        case RecoveryCompleted =>  {
            log.info("UserManager recovery completed")
            /**
             * Recover all child user actors
             */ 
            users.foreach(userActor(_))
        }
    }
    
    /**
     * Update state in response to events
     */ 
    val updateState: UserManagerEvent => Unit = {
        case UserAdded(userId: String) => users += userId
    }
    
    val receiveCommand: Receive = {
        case m @ UserItemInteraction(userId, itemId) =>
            /**
             * Send interaction to child user actor
             */
             userActor(userId) forward m
             
             persist(UserAdded(userId))(updateState)
             
        case GetNumUsers =>
            sender() ! NumUsers(users.size)
    }
    
    /**
     * Get ActorRef for child user actor
     */     
    def userActor(userId: String) = {
        context.child(userActorName(userId)).getOrElse {
            context.actorOf(UserActor.props(userId), userActorName(userId))
        }
    }
    
    /**
     * Get name for the user actor based on user id
     */
    def userActorName(userId: String) = "User:"+userId
}
