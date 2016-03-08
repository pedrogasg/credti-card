package poc.credit

import scala.collection.immutable.HashMap
import akka.actor.Actor
import akka.actor.ActorRef

class AccountManager extends Actor {
  import AccountManager._
  var accounts = HashMap.empty[String,ActorRef]
  
  def receive = {
    case AccountRegistration(id) =>
      accounts = accounts + (id -> sender())
  }
}

object AccountManager {
  case class AccountRegistration(id:String)
}