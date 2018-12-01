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
}
