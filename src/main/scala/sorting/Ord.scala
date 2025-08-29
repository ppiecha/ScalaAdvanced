package sorting

trait Ord[A] {
  def lte(a1: A, a2: A): Boolean
  extension (a: A) {
    def <=(another: A): Boolean = lte(a, another)
  }
}

object Ord {
  given Ord[Int] with
    def lte(a1: Int, a2: Int): Boolean = a1 <= a2
  given Ord[Char] with
    def lte(a1: Char, a2: Char): Boolean = a1 <= a2  
}
