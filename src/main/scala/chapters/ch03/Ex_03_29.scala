package chapters.ch03

import lib.{Tree, Leaf, Branch}

//  def max[B >: A](implicit num: Numeric[B]): Int = {
//    println("GOT INTO max: num = " + num)
//    println("this = " + this)
//
//    //println("           numInt = " + num.toInt)
//    //println("           numInt = " + num.asInstanceOf[Int])
//    //println("this = " + this)
//    this match {
//      case Leaf(x) => num.toInt(x)
//      case Branch(left, right) => {
//        println("left = " + left)
//        //println("left.max = " + left.max)
//        println("right = " + right)
//        //println("right.max = " + right.max)
//        0
//      }
//    }
//  }
//  //def max[A <: Int]: Int = fold(this.map(_.), Int.MinValue)((x, s) => if (x > s) x else s)


object Ex_03_29 extends App {
  println("Ex_03_29")

  val t0 = Leaf(26)
  val t1 = Leaf(66)
  val t2 = Branch(t0, t1)
  val t3 = Leaf(29)
  val t4 = Leaf(71)
  val t5 = Branch(t4, t3)
  val t6 = Leaf(99)
  val t7 = Branch(t6, t5)
  val t8 = Branch(t2, t7)

  t8.print
  println(s"Size of t8 is ${t8.size}")
  //println(s"Maxi of t0 is ${t0.max}")
  //println(s"Maxi of t8 is ${t8.max}")
}
