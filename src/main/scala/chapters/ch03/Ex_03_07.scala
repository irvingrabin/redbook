package chapters.ch03

import library._

object Ex_03_07 extends App {
  println("Ex_03_07")

  val sumTestCases = Seq(
    (List(1, 2, 3, 4), 10),
    (List(1, 2), 3),
    (List(), 0),
  )

  val productTestCases = Seq(
    (List(1.0, 2.0, 3.0, 4.0), 24.0),
    (List(1.0, 2.0), 2.0),
    (List(1.0, 2.0, 0.0, 5.0, 8.0, 11.0), 0.0),
    (List(), 1.0),
  )

  sumTestCases.map {
    case (source, result) => {
      println("SUM TESTCASE")
      source.print("Source")
      println(s"    Expected Result = $result")
      println(s"        1-st Result = ${List.sum(source)}")
      println(s"        2-nd Result = ${List.sum2(source)}")
    }
  }

  productTestCases.map {
    case (source, result) => {
      println("PRODUCT TESTCASE")
      source.print("Source")
      println(s"    Expected Result = $result")
      println(s"        1-st Result = ${List.product(source)}")
      println(s"        2-nd Result = ${List.product2(source)}")
      println(s"        3-rd Result = ${List.product3(source)}")
    }
  }

}
