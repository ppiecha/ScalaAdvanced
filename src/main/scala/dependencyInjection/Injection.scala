package dependencyInjection

import cats._
import cats.data._

object Injection extends App {

  val parityReader: Reader[Int, String] = Reader(n => if n % 2 == 0 then "even" else "odd")
  //parityReader.run(2)

  case class Person(id: Long, name: String, email: String)

  case class Account(id: Long, ownerId: Long)

  trait AccountRepository {
    val accountRepository: AccountService

    trait AccountService {
      def findAccountById(id: Long): Account

      // 5
      def saveAccount(account: Account): Unit
    }
  }

  trait LiveAccountRepository extends AccountRepository {
    override val accountRepository: AccountService = new AccountService {
      override def findAccountById(id: Long): Account = Account(id, 0)

      // 6
      override def saveAccount(account: Account): Unit = ()
    }
  }

  trait PersonRepository {
    val personRepository: PersonService

    trait PersonService {
      def findPersonById(id: Long): Person
    }
  }

  trait LivePersonRepository extends PersonRepository {
    override val personRepository: PersonService = new PersonService {
      override def findPersonById(id: Long): Person = Person(0, "found", "address@example.com")
    }
  }

  def findNextAccount(id: Long): Reader[AccountRepository, Account] =
    for {
      ar <- Reader(identity[AccountRepository])
      account = ar.accountRepository.findAccountById(id + 1)
    } yield account

  def findOwnerNameByAccountId(id: Long): Reader[Env, String] =
    for {
      ar <- Reader(identity[AccountRepository])
      account = ar.accountRepository.findAccountById(id)
      pr <- Reader(identity[PersonRepository])
      person = pr.personRepository.findPersonById(account.ownerId)
    } yield person.name

  val liveEnv: Env = new LiveAccountRepository with LivePersonRepository with LiveEmailService
  val name = findOwnerNameByAccountId(0).run(liveEnv)
  println(name)

  // 1
  trait EmailRepository {
    val email: EmailService

    trait EmailService {
      def sendEmail(address: String, text: String): Unit
    }
  }

  // 2
  trait LiveEmailService extends EmailRepository {
    override val email: EmailService = (a, t) => ()
  }

  // 3
  type Env = AccountRepository with PersonRepository with LiveEmailService

  // 7
  def openAccount(accountId: Long, ownerId: Long): Reader[Env, Account] =
    for {
      ar <- Reader(identity[AccountRepository])
      pr <- Reader(identity[PersonRepository])
      owner = pr.personRepository.findPersonById(ownerId)
      er <- Reader(identity[EmailRepository])
    } yield {
      val account = Account(accountId, ownerId)
      ar.accountRepository.saveAccount(account)
      er.email.sendEmail(owner.email, "account opened")
      account
    }
//  def openAccount2(accountId: Long, ownerId: Long): Reader[Env, Unit] =
//    for {
//      ar <- Reader(identity[AccountRepository])
//      pr <- Reader(identity[PersonRepository])
//      owner = pr.personRepository.findPersonById(ownerId)
//      er <- Reader(identity[EmailRepository])
//      account = Account(accountId, ownerId)
//      _ = ar.accountRepository.saveAccount(account)
//      _ = er.email.sendEmail(owner.email, "account opened")
//    } do ()
  val ac = openAccount(1, 0).run(liveEnv)
  println(ac)

}
