package poc.credit

import scala.collection.immutable.HashMap
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Stash

class AccountManager extends Actor with Stash{
  import Account._
  import AccountManager._
  import AccountReviver._
  var accounts = HashMap.empty[String,ActorRef]
  val manager = system.actorOf(Props[AccountManager], name = "manager")
  def receive = {
    case AddOperation(id,operation) =>
      accounts get id match {
        Some(ref) => ref ! operation
        None =>
          stash()
          manager ! Rise(id)
      }
    case AccountRegistration(id) =>
      accounts = accounts + (id -> sender())
      unstashAll()
  }
}

object AccountManager {
  import Account._
  import AccountManager._
  case class AccountRegistration(id:String)
  case class AddOperation(id:String, operation:AccountOperation)
}
