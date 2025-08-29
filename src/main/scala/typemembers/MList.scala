package typemembers

trait MList {
  type A
  def head: A
  def tail: MList
}

class StringList(hd: String, tl: StringList) extends MList {
  type A = String
  def head: A = hd
  def tail: MList = tl
}

class IntList(hd: Int, tl: IntList) extends MList {
  type A = Int
  def head: A = hd
  def tail: MList = tl
}