package exception

object handle extends App{

  def half(n: Int): Int =
    if n % 2 == 0 then n/2
    else throw new RuntimeException("wrong value")

  def div(a: Int, b: Int): Int =
    try {
      a/b
    } catch {
      case e: Throwable => e.printStackTrace(); -100
    } finally{
      println("finally")
    }

  println(div(2, 0))
  println(div(10, 2))

}
