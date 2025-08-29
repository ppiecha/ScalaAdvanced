package channel

trait Channel {
  def read[A]()(implicit dec: ByteDecoder[A]): A
}

trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}

object ByteDecoder extends App {
  def apply[A](bytes: Array[Byte])(implicit dec: ByteDecoder[A]): Option[A] = dec.decode(bytes)
  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = new ByteDecoder[A] {
    override def decode(bytes: Array[Byte]): Option[A] = f(bytes)
  }
  implicit val stringDecoder: ByteDecoder[String] =
    instance[String](arr => Some(arr.map(c => String.valueOf(c.toChar)).mkString))

  println(ByteDecoder[String](Array(71, 57)))
  println(ByteDecoder[String](Array(98, 105, 101, 110, 32, 58, 41)))

}


