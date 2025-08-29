package concurrency

case class HelloThread(maxThreads: Int, counter: Int) extends Thread {
  println(s"Thread $counter created")
  if counter <= maxThreads then {
    val newOne = HelloThread(maxThreads, counter + 1)
    newOne.start()
    newOne.join()
  }
  println(s"Hello from thread $counter")
}

object RunTest extends App {
  val test = HelloThread(10, 1)
}




