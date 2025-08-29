package concurrency

import scala.util.Random

object ProdCons extends App {
  class Container (maxCount: Int = 10) {
    var container = List[Int]()
    def isEmpty = container.isEmpty
    def isFull = container.size >= maxCount
    def add(n: Int) = container = n :: container
    def get = {
      val head = container.head
      container = container.tail
      head
    }
  }
  def prodCons1 = {

    val container = new Container()
    val rnd = new Random()

    def producer() = new Thread(() => {
      println("[producer] started...")
      while (true) {
        container.synchronized {
          while (container.isFull) {
            println(s"[producer] container is full ${container.container}")
            container.wait()
          }
          container.add(rnd.nextInt(100))
          container.notify()
        }
        Thread.sleep(rnd.nextInt(500))
      }
    })

    def consumer() = new Thread(() => {
      println("[consumer] started")
      while (true) {
        container.synchronized {
          while (container.isEmpty) {
            println("[consumer] container is empty")
            container.wait()
          }
          println(s"[consumer] consumed ${container.get}")
          container.notify()
        }
        Thread.sleep(rnd.nextInt(500))
      }
    })
    producer().start()
    producer().start()
    consumer().start()
    consumer().start()
  }
  prodCons1
}
