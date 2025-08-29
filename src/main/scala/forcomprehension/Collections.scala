package forcomprehension

object Collections extends App {

  val arr = Array(1, 2, 3, 4)
  val res1 = for a <- arr yield a * a
  println(res1.toList)
  for a <- arr do
    println(a)
  val filtered = for i <- 1 to 4 if i % 2 == 0
    yield i
  println(filtered)
  val pairs =
    for {
      letter <- Seq('a', 'b', 'c')
      number <- Seq('1', '2', '3') if number % 2 == 0
    } yield (letter, number)
  println(pairs)
}
