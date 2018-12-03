package chapters.ch04

import org.scalatest.{FlatSpec, Matchers}
import chapters.ch03.{List}

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
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

    variance(Seq()) shouldBe None
    variance(Seq(1.0)) shouldBe Some(0.0)
    variance(Seq(1.0, 1.0)) shouldBe Some(0.0)
    variance(Seq(1.0, 3.0)) shouldBe Some(1.0)
    variance(Seq(3.0, 6.0, 9.0)) shouldBe Some(6.0)
  }

  it should "04_03 Implement map2 for options" in {
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      for { av <- a; bv <- b } yield f(av, bv)

    map2(Some(5), Some(7))(_ + _) shouldBe Some(12)
    map2[Int, Int, Int](None, Some(7))(_ + _) shouldBe None
    map2(Some(5), None)(_ + _) shouldBe None
    map2[Int, Int, Int](None, None)(_ + _) shouldBe None
  }

  it should "04_04 implement sequence" in {
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      val a1 = a.filter(_.exists).map(_.get)
      if (a1.length > 0) Some(a1) else None
    }

    sequence(List()) shouldBe None
    sequence(List(None, None, None, None)) shouldBe None
    sequence(List(Some(1), None, Some(2), Some(3), None, None, Some(4), None)) shouldBe Some(List(1, 2, 3, 4))
  }

  it should "04_05 implement traverse" in {
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      val a1 = a.map(f).filter(_.exists).map(_.get)
      if (a1.length > 0) Some(a1) else None
    }

    traverse(List[Int]())(x => Some(x + 2)) shouldBe None
    traverse(List(1,2,3,4,5,6,7,8))(x => if (x % 3 == 0) None else Some(x)) shouldBe Some(List(1, 2, 4, 5, 7, 8))
    traverse(List(1,2,3,4,5,6,7,8))(x => if (x < 10) None else Some(x)) shouldBe None
  }

  it should "04_06 implement Either correctly" in {
    val e1: Seq[Either[String, Int]] = Seq(Left("hello"), Right(25), Right(44), Left("what"), Right(56))

    e1.map(x => x.map(100.0 - _.toDouble)) shouldBe Seq(Left("hello"), Right(75.0), Right(56.0), Left("what"), Right(44.0))

    e1.map(
      x => x.flatMap(x => if (x % 2 == 0) Right(99 - x) else Left(s"$x is an odd number"))
    ) shouldBe Seq(Left("hello"), Left("25 is an odd number"), Right(55), Left("what"), Right(43))

    Right(43).orElse(Right("xyz")) shouldBe Right(43)
    Left(43).orElse(Right("xyz")) shouldBe Right("xyz")

    Right(43).map2(Right(7.34))(_.toDouble + _) shouldBe Right(50.34)
    Right(45).map2(Left(3.62))(_.toDouble + _) shouldBe Left(3.62)
    (Left(45): Either[Int, Double]).map2(Right(3.62): Either[Double, Double])(_ + _) shouldBe Left(45)
    (Left(45): Either[Int, Double]).map2(Left(3.62): Either[Double, Double])(_ + _) shouldBe Left(45)
  }

  it should "04_07 implement sequence and traverse for Either" in {
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      val rights = as.map(f).flatMap(_ match {
        case Right(b) => List(b)
        case Left(_)  => List.empty
      })
      if (rights.length > 0) Right(rights)
      else {
        val lefts = as.map(f).flatMap(_ match {
          case Right(_) => List.empty
          case Left(l)  => List(l)
        })
        if (lefts.length > 0) Left(lefts(0).get)
        else Right(List.empty)
      }
    }

    traverse(List(1, 2, 3, 4, 5))((x: Int) => {
      if (x % 2 == 1) Left(s"$x is an odd number")
      else Right(x/2)
    }) shouldBe Right(List(1, 2))
    traverse(List(1, 2, 3, 4, 5))((x: Int) => {
      if (x < 10) Left(s"$x is smaller than 10")
      else Right(x)
    }) shouldBe Left("1 is smaller than 10")

  }

  it should "04_08 have map2 return both all errors" in {
    // TODO: eventually fill up
  }
}
