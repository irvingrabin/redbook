package chapters.ch02

object Ex_02_02 extends App {
  println("Ex_02_02")

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def _isSorted(as: Array[A], ordered: (A, A) => Boolean, pos: Int): Boolean = {
      if (pos >= as.length -1) true
      else if (!ordered(as(pos), as(pos+1))) false
      else _isSorted(as, ordered, pos+1)
    }
    _isSorted(as, ordered, 0)
  }

  def ascInt(a: Int, b: Int): Boolean = a <= b
  def descInt(a: Int, b: Int): Boolean = a >= b


  println("chapters.chapter02.Ex2_2")
  Seq(
    (Array(4, 6, 4), ascInt(_,_) ),
    (Array(4, 6, 6), ascInt(_,_) ),
    (Array(6, 6, 4), descInt(_,_) ),
    (Array(4, 6, 23), ascInt(_,_) ),
    (Array(4, 6, -23), ascInt(_,_) ),
    (Array(4), ascInt(_,_) ),
    (Array(4), descInt(_,_) ),
    (Array[Int](), ascInt(_,_) ),
    (Array[Int](), descInt(_,_) )
  ).map {
      case (a: Array[Int], o) => println(
        "array " + a.toList + " is " + (if (isSorted(a, o)) "" else "not ") + "sorted")
  }
}