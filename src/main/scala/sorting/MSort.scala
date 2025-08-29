package sorting

object MSort extends App{

  def mIntSort(list: List[Int]): List[Int] = {
    def merge(l1: List[Int], l2: List[Int]): List[Int] = {
      if l1.isEmpty then l2
      else if l2.isEmpty then l1
      else if l1.head <= l2.head then l1.head :: merge(l1.tail, l2)
      else l2.head :: merge(l1, l2.tail)
    }
    if list.isEmpty then Nil
    else if list.tail.isEmpty then list
    else {
      val (l1, l2) = list.splitAt(list.length / 2)
      merge(mIntSort(l1), mIntSort(l2))
    }
  }

  def mSort[A: Ord](list: List[A]): List[A] = {
    def merge(l1: List[A], l2: List[A]): List[A] = {
      if l1.isEmpty then l2
      else if l2.isEmpty then l1
      else if l1.head <= l2.head then l1.head :: merge(l1.tail, l2)
      else l2.head :: merge(l1, l2.tail)
    }

    if list.isEmpty then Nil
    else if list.tail.isEmpty then list
    else {
      val (l1, l2) = list.splitAt(list.length / 2)
      merge(mSort(l1), mSort(l2))
    }
  }

  println(mIntSort(List(1,4,3,2)))
  assert(mSort(List('A', 'F', 'C', 'B')) == List('A', 'B', 'C', 'F'))
}
