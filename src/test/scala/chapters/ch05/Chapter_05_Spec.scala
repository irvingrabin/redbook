package chapters.ch05

import org.scalatest.{FlatSpec, Matchers}
import chapters.ch03.List

class Chapter_05_Spec  extends FlatSpec with Matchers {
  it should "05_01 convert stream into a list" in {
    Stream(1, 2, 3, 4, 5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  it should "05_02 implement take(n) and drop(n)" in {
    val s1 = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)
    s1.take(5) shouldEqual Seq(1, 2, 3, 4, 5)
    s1.drop(5) shouldBe a [Stream[_]]
    s1.drop(5).toList shouldEqual List(6, 7, 8, 9)
  }

  it should "05_03 " in {
  }

  it should "05_04 " in {
  }

  it should "05_05 " in {
  }

  it should "05_06 " in {
  }

  it should "05_07 " in {
  }

  it should "05_08 " in {
  }
}
