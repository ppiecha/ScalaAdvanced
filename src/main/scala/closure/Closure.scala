package closure

object Closure extends App{
  def outer(s1: String, s2: String) =
    def inner(prefix: String): String =
      s"$prefix $s1 $s2"
    inner
  val fun1 = outer("s1", "s2")
  println(fun1("prefix"))
}
