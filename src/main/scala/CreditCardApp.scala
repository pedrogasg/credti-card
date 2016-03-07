import poc.credit._
import akka.persistence._
import akka.actor.{ Actor, ActorRef, ActorSystem, Props }

object CreditCardApp extends App {
  import Account._
  
  val system = ActorSystem("fist-acount-test")
  
  val account = system.actorOf(Props(new Account("test-id")))
  
  account ! AccountOperation(100,CreditTransaction)
  account ! AccountOperation(100,DebitTransaction)
  account ! AccountOperation(100,CreditTransaction)
  account ! AccountOperation(100,CreditTransaction)
  account ! AccountOperation(100,DebitTransaction)
  
  Thread.sleep(1000)
  
  system.terminate()
}