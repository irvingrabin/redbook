package chapters.ch02

import chapters.ch02.lib.CommonDefs.{
  fib, ascInt, descInt, isSorted, curry, uncurry, compose}
import org.scalatest.{FlatSpec, Matchers}

class Chapter02Spec extends FlatSpec with Matchers {

  it should "run test 02_01" in {
    fib(0) shouldBe 1
    fib(3) shouldBe 6
    fib(6) shouldBe 720
  }

  it should "run test 02_02" in {
    isSorted(Array(4, 6, 4), ascInt(_, _)) shouldBe false
    isSorted(Array(4, 6, 4), descInt(_, _)) shouldBe false

    isSorted(Array(4, 6, 6), ascInt(_, _)) shouldBe true
    isSorted(Array(4, 6, 6), descInt(_, _)) shouldBe false

    isSorted(Array(6, 6, 4), ascInt(_, _)) shouldBe false
    isSorted(Array(6, 6, 4), descInt(_, _)) shouldBe true

    isSorted(Array(4), ascInt(_, _)) shouldBe true
    isSorted(Array(4), descInt(_, _)) shouldBe true

    isSorted(Array[Int](), ascInt(_, _)) shouldBe true
    isSorted(Array[Int](), descInt(_, _)) shouldBe true
  }

  it should "run test 02_03" in {
    val add = (a: Int, b: Int) => a + b
    val multiply = (a: Int, b: Int) => a * b

    curry(add)(5)(6) shouldEqual add(5, 6)
    curry(multiply)(5)(6) shouldEqual multiply(5, 6)
  }

  it should "run test 02_04" in {
    val addCurry = (a: Int) => (b: Int) => a + b
    val multiplyCurry = (a: Int) => (b: Int) => a * b
    assert(uncurry(addCurry)(5, 6) == addCurry(5)(6))
    assert(uncurry(multiplyCurry)(5, 6) == multiplyCurry(5)(6))
  }

  it should "run test 02_05" in {
    val f1 = (a: Int) => a + 5
    val f2 = (a: Int) => a * 3
    assert(compose(f1, f2)(5) == 20)
    assert(compose(f2, f1)(5) == 30)
  }
}
