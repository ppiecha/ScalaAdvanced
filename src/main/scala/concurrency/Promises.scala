package concurrency

import scala.concurrent.Promise
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

object Promises extends App {

  val p = Promise[Int]()
  val f = p.future
  f.onComplete({
    case Success(i) => println(i)
    case Failure(e) => e.printStackTrace()
  })
  println("test")
  p.success(42)
}
