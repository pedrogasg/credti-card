package poc.credit
import scala.reflect._
import akka.persistence._
import akka.persistence.fsm._
import akka.persistence.fsm.PersistentFSM.FSMState
import akka.persistence.fsm.PersistentFSM
import akka.actor.Stash

object Account {
  // States 
  sealed trait AccountState extends FSMState
  case object Inactive extends AccountState{
    override def identifier = "Inactive"
  }
  case object Active extends AccountState{
    override def identifier = "Active"
  }
  case object Blocked extends AccountState{
    override def identifier = "Blocked"
  }
  
  // Funds
  
  sealed trait Funds{
    val balance : BigDecimal
    val overdraftLimit : BigDecimal
  }
  case object NotBalance extends Funds{
    override val balance:BigDecimal = 0;
    override val overdraftLimit:BigDecimal = 0;
  }
  case class PositiveBalance(override val balance:BigDecimal, override val overdraftLimit:BigDecimal) extends Funds
  case class NegativeBalance(override val balance:BigDecimal, override val overdraftLimit:BigDecimal) extends Funds
  
  // Domain Events
  
  sealed trait DomainEvent
	case class AcceptedTransaction(amount: BigDecimal,`type`: TransactionType) extends DomainEvent
	case class RejectedTransaction(amount: BigDecimal,`type`: RejectionCause) extends DomainEvent
	
	// Transaction
	sealed trait TransactionType
	case object CreditTransaction extends TransactionType
	case object DebitTransaction extends TransactionType
	// Rejection Cause
	
	sealed trait RejectionCause
	case object InactiveAccount extends RejectionCause
	case object NotCoverOperation extends RejectionCause
	case object BlockedAccount extends RejectionCause
	
	case object UnblockAccount
	case object BlockAccount
	case class AccountOperation(amount:BigDecimal,`type`:TransactionType)
}

class Account(val id:String) extends PersistentFSM[Account.AccountState, Account.Funds,Account.DomainEvent] with Stash{
  import Account._
  
  override def persistenceId:String = id
  
  def absoluteBalance(balance:BigDecimal,overdraftLimit:BigDecimal) = {
    println(s"new balance ${balance}")
    if(balance > 0)
      PositiveBalance(balance, overdraftLimit)
    else
      NegativeBalance(balance, overdraftLimit)
  }
  override def applyEvent(e:DomainEvent,currentFunds:Funds):Funds = {
    e match{
      case AcceptedTransaction(amount,CreditTransaction) =>
        val total = currentFunds.balance + amount
        absoluteBalance(total,currentFunds.overdraftLimit)
      case AcceptedTransaction(amount,DebitTransaction) =>
        val total = currentFunds.balance - amount
        absoluteBalance(total,currentFunds.overdraftLimit)
      case RejectedTransaction(_,reason) =>
        println(s"rejected ${reason}")
        currentFunds
    }
  }
  override def domainEventClassTag: ClassTag[DomainEvent] = classTag[DomainEvent]
  
  when(Inactive){
    case Event(AccountOperation(amount,CreditTransaction),_) =>
      //stash()
      goto(Active) applying AcceptedTransaction(amount,CreditTransaction)
    case Event(AccountOperation(amount,_),_) =>
      stay applying RejectedTransaction(amount,InactiveAccount)
  }
  
  when(Active){
    case Event(AccountOperation(amount,CreditTransaction),funds) =>
      stay applying AcceptedTransaction(amount,CreditTransaction)
    case Event(AccountOperation(amount,DebitTransaction),funds) =>
      val sum = funds.balance - amount
      if(sum >= funds.overdraftLimit)
        stay applying AcceptedTransaction(amount,DebitTransaction)
      else
        stay applying RejectedTransaction(amount,NotCoverOperation)
    case Event(BlockAccount,_) => 
        goto(Blocked)
    case Event(UnblockAccount,_) =>
        stay
  }
  
  when(Blocked){
    case Event(BlockAccount,_) => 
      stay
    case Event(UnblockAccount,_) =>
      goto(Active)
    case Event(AccountOperation(amount,_),_) =>
      stay applying RejectedTransaction(amount,BlockedAccount)
  }
  
  onTransition {
    case Inactive -> Active => 
      //unstashAll()
  }
  startWith(Inactive,NotBalance)
}