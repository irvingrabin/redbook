package chapters.ch10

import org.scalatest.{FlatSpec, Matchers}

class Chapter_10_Spec extends FlatSpec with Matchers {
  it should "10-01 implement monoids" in {
    val intAddition = new Monoid[Int] {
      val zero: Int = 0
      override def op(a1: Int, a2: Int): Int = a1 + a2
    }
    val intMultiplication = new Monoid[Int] {
      val zero: Int = 1
      override def op(a1: Int, a2: Int): Int = a1 * a2
    }
    val booleanAnd = new Monoid[Boolean] {
      val zero: Boolean = true
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    }
    val booleanOr = new Monoid[Boolean] {
      val zero: Boolean = false
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    }

    intAddition(5, 7) shouldBe 12
    intAddition.zero shouldBe 0
    intAddition(10, intAddition.zero) shouldBe 10
    intAddition(intAddition.zero, 10) shouldBe 10
    intAddition(5, 6, 7, 8, 9, 10) shouldBe 45

    intMultiplication(5, 6) shouldBe 30
    intMultiplication.zero shouldBe 1
    intMultiplication(10, intMultiplication.zero) shouldBe 10
    intMultiplication(intMultiplication.zero, 10) shouldBe 10
    intMultiplication(3, 4, 5, 6) shouldBe 360

    booleanAnd(true, false) shouldBe false
    booleanAnd.zero shouldBe true
    booleanAnd(true, booleanAnd.zero) shouldBe true
    booleanAnd(booleanAnd.zero, true) shouldBe true
    booleanAnd(false, booleanAnd.zero) shouldBe false
    booleanAnd(booleanAnd.zero, false) shouldBe false
    booleanAnd(true, true, true) shouldBe true

    booleanOr(true, false) shouldBe true
    booleanOr.zero shouldBe false
    booleanOr(true, booleanOr.zero) shouldBe true
    booleanOr(booleanOr.zero, true) shouldBe true
    booleanOr(false, booleanOr.zero) shouldBe false
    booleanOr(booleanOr.zero, false) shouldBe false
    booleanOr(false, false, false) shouldBe false
  }

  it should "10_02 define an option Monoid" in {
    def optionMonoidFirst[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 match {
        case (Some(a1v)) => a1
        case _           => a2
      }
      val zero: Option[A] = None
    }
    optionMonoidFirst(Some(5), Some(6)) shouldBe Some(5)
  }

  it should "10_03 define an endofunction Monoid" in {

  }

  it should "10_04 property test on Monoids" in {

  }

  it should "10_05 implement foldMap" in {

  }

  it should "10_06 implement foldLeft and foldRight using FoldMap" in {

  }

  it should "10_07 implement foldMap for IndexedSeq" in {

  }
}
