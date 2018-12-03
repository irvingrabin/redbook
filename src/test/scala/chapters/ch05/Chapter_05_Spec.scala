package chapters.ch05

import org.scalatest.{FlatSpec, Matchers}
import chapters.ch03.List

class Chapter_05_Spec  extends FlatSpec with Matchers {
  it should "05_01 convert stream into a list" in {
    Stream(1, 2, 3, 4, 5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  it should "05_02 define and test variance" in {
  }

  it should "05_03 Implement map2 for options" in {
  }

  it should "05_04 implement sequence" in {
  }

  it should "05_05 implement traverse" in {
  }

  it should "05_06 implement Either correctly" in {
  }

  it should "05_07 implement sequence and traverse for Either" in {
  }

  it should "05_08 have map2 return both all errors" in {
  }
}
