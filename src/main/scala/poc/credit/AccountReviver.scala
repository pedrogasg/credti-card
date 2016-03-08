package poc.credit
import akka.actor.Actor
class AccountReviver extends Actor{
  import AccountReviver._
  import Account._
  def receive = {
    case Rise(id) =>
      val account = system.actorOf(Account.props(id),id)
  }
}

object AccountReviver {
  case class Rise(id:string)
}
