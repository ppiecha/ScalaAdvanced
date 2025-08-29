package sorting

object ISort extends App {

  def intSort(list: List[Int])(lte: (Int, Int) => Boolean): List[Int] =
    if list.isEmpty then Nil
    else intInsert(list.head, intSort(list.tail)(lte))(lte)

  def intInsert(i: Int, list: List[Int])(lte: (Int, Int) => Boolean): List[Int] = {
    if list.isEmpty then List(i)
    else if lte(i, list.head) then i :: list
    else list.head :: intInsert(i, list.tail)(lte)
  }

  def genSort[A](list: List[A])(lte: (A, A) => Boolean): List[A] =
    if list.isEmpty then Nil
    else genInsert(list.head, genSort(list.tail)(lte))(lte)

  def genInsert[A](i: A, list: List[A])(lte: (A, A) => Boolean): List[A] = {
    if list.isEmpty then List(i)
    else if lte(i, list.head) then i :: list
    else list.head :: genInsert(i, list.tail)(lte)
  }

  def iSort[A: Ord](list: List[A]): List[A] =
    if list.isEmpty then Nil
    else iInsert(list.head, iSort(list.tail))

  def iInsert[A: Ord](i: A, list: List[A]): List[A] = {
    if list.isEmpty then List(i)
    else if i <= list.head then i :: list
    else list.head :: iInsert(i, list.tail)
  }

  assert(intSort(List(2, 1, 4, 3))(_ <= _) == List(1, 2, 3, 4))
  assert(genSort(List("Ania", "Piotr", "MoMo", "Leos"))(_ <= _) == List("Ania", "Leos", "MoMo", "Piotr"))
  assert(iSort(List('A', 'F', 'C', 'B')) == List('A', 'B', 'C', 'F'))
  assert(iSort(List(2, 1, 4, 3)) == List(1, 2, 3, 4))
  //assert(iSort(List("Ania", "Piotr", "MoMo", "Leos")) == List("Ania", "Leos", "MoMo", "Piotr"))
  println("end of test")
}
