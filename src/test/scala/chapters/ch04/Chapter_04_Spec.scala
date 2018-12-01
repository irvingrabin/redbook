package chapters.ch04

import org.scalatest.{FlatSpec, Matchers}
import chapters.ch04.lib.{Option, Some, None}

class Chapter_04_Spec  extends FlatSpec with Matchers {
  it should "04_01 implement basic Options" in {
    Some(5).map(_.toString) shouldBe Some("5")
    Some(40).map(x => x * 5) shouldBe Some(200)
    (None: Option[Int]).map(x => x * 5) shouldBe None

    Some(5).flatMap(x => Some(x.toString)) shouldBe Some("5")
    Some(40).flatMap(x => Some(x * 5)) shouldBe Some(200)
    (None: Option[Int]).flatMap(x => Some(x * 5)) shouldBe None

    Some(5).getOrElse(7) shouldBe 5
    None.getOrElse(7) shouldBe 7

    Some(5).orElse(Some(7)) shouldBe Some(5)
    None.orElse(Some(7)) shouldBe Some(7)
    None.orElse(None) shouldBe None

    Some(5).filter(_ % 2 == 1) shouldBe Some(5)
    Some(5).filter(_ % 2 == 0) shouldBe None
    (None: Option[Int]).filter(_ => true) shouldBe None
  }

  it should "04_02 define and test variance" in {
    // TODO: implement with flatMap
    def variance(xs: Seq[Double]): Option[Double] = {
      if (xs.size == 0) None
      else {
        val mean: Double = xs.sum / xs.size
        val variance = xs.foldLeft(0.0)((s, v) => s + (v - mean)*(v - mean)) / xs.size
        Some(variance)
      }
    }
    variance(Seq()) shouldBe None
    variance(Seq(1.0)) shouldBe Some(0.0)
    variance(Seq(1.0, 1.0)) shouldBe Some(0.0)
    variance(Seq(1.0, 3.0)) shouldBe Some(1.0)
    variance(Seq(3.0, 6.0, 9.0)) shouldBe Some(6.0)
  }
}
