package io.beehive.actors

import akka.actor.Actor
import akka.actor.Props
import io.beehive.lib.Akka

object UserManager {
    /**
     * Create one UserManager actor instance which will be used everywhere
     */ 
    val userManager = Akka.actorSystem.actorOf(Props(classOf[UserManager]), "UserManager")
    
    case object GetNumUsers
    case class NumUsers(num: Int)
}

class UserManager extends Actor {
    
    import UserActor._
    import UserManager._
    
    /**
     * Set of users
     */ 
    val users = scala.collection.mutable.Set[Int]()
    
    def receive = {
        case m @ UserItemInteraction(userId, itemId) =>
            /**
             * Send interaction to child user actor
             */
             userActor(userId) forward m
             
             /**
              * Add user to users set
              */ 
             users += userId
             
        case GetNumUsers =>
            sender() ! NumUsers(users.size)
    }
    
    /**
     * Get ActorRef for child user actor
     */     
    def userActor(userId: Int) = {
        context.child(userActorName(userId)).getOrElse {
            context.actorOf(UserActor.props(userId), userActorName(userId))
        }
    }
    
    /**
     * Get name for the user actor based on user id
     */
    def userActorName(userId: Int) = "User:"+userId.toString()
}
