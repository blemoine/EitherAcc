package co.sachemmolo.eitheracc

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import shapeless.{:+:, CNil, Coproduct, Inl, Poly1}

class EitherAccSpec extends WordSpec with Matchers with PropertyChecks {

  def genSuccessAcc[A, S <: Coproduct, I](geni: Gen[I]): Gen[EitherAcc[A :+: S, I]] = geni.map(i => EitherAcc.pure[A, I](i).widen[A :+: S])

  def genErrAcc[A, S <: Coproduct, I](gena: Gen[A]): Gen[EitherAcc[A :+: S, I]] = gena.map(a => EitherAcc.err[A, I](a).widen[A :+: S])

  def genEitherAcc[A, S <: Coproduct, I](geni: Gen[I], gena: Gen[A]): Gen[EitherAcc[A :+: S, I]] = Gen.oneOf(genSuccessAcc[A, S, I](geni), genErrAcc[A, S, I](gena))

  implicit def arbEitherAcc[A, S <: Coproduct, I](implicit arbi: Arbitrary[I], arba: Arbitrary[A]):Arbitrary[EitherAcc[A :+: S, I]] = Arbitrary(genEitherAcc[A, S, I](arbi.arbitrary, arba.arbitrary))

  "EitherAcc" should {
    "be able to construct a pure value" in {
      forAll { (i: Int) =>
        EitherAcc.pure(i) should matchPattern {
          case Success(j) if j == i =>
        }
      }
    }

    "be able to construct an error" in {
      forAll { (i: Int) =>
        val either: EitherAcc[Int :+: CNil, Nothing] = EitherAcc.err(i)
        either.isError shouldBe true
      }
    }

    "construct a success from a right" in {
      forAll { (i:Int) =>
        val r:EitherAcc[String :+: CNil, Int] = EitherAcc.fromEither(Right(i))

        r shouldBe Success(i)
      }
    }

    "construct an error from a left" in {
      forAll { (s:String) =>
        val r:EitherAcc[String :+: CNil, Int] = EitherAcc.fromEither(Left(s))

        r match {
          case Success(value) => fail("The generated EitherAcc should be an error")
          case Err(e) => e.select[String] shouldBe Some(s)
        }
      }
    }
  }

  "isError" should {
    "return true if the EitherAcc is an error" in {
      forAll(genErrAcc[String, CNil, Int](Arbitrary.arbitrary[String])) { (e:EitherAcc[String :+: CNil, Int]) =>
        e.isError shouldBe true
      }
    }

    "return false if the EitherAcc is a success" in {
      forAll(genSuccessAcc[String, CNil, Int](Arbitrary.arbitrary[Int])) { (e:EitherAcc[String :+: CNil, Int]) =>
        e.isError shouldBe false
      }
    }
  }

  "fold" should {
    "return the value is EitherAcc is a success" in {
      object getLength extends Poly1 {
        implicit def caseString = at[String](_.length)
      }
      forAll { (i:Int) =>
        EitherAcc.pure[String, Int](i).fold(getLength, identity) shouldBe i
      }
    }

    "return the error is EitherAcc is an error" in {
      object getLength extends Poly1 {
        implicit def caseString = at[String](_.length)
      }
      forAll { (s:String) =>
        EitherAcc.err[String, Int](s).fold(getLength, identity) shouldBe s.length
      }
    }
  }

  "map" should {
    "respect identity" in {
      forAll { (e: EitherAcc[String :+: CNil, Int]) =>
        e.map(identity) shouldBe e
      }
    }
    "be associative" in {
      forAll { (e: EitherAcc[String :+: CNil, Int], f: Int => Boolean, g: Boolean => String) =>
        e.map(f).map(g) shouldBe e.map(f.andThen(g))
      }
    }
  }

  "flatmap" should {
    "respect right identity" in {
      forAll { (e: EitherAcc[String :+: CNil, Int]) =>
        e.flatMap(i => EitherAcc.pure[String, Int](i)) shouldBe e
      }
    }
    "respect left identity" in {
      forAll { (i: Int, f: Int => EitherAcc[String :+: CNil, Int]) =>
        EitherAcc.pure[String, Int](i).flatMap(f) shouldBe f(i)
      }
    }
    "be associative" in {
      forAll { (m: EitherAcc[String :+: CNil, Int], f: Int => EitherAcc[String :+: CNil, Boolean], g: Boolean => EitherAcc[String :+: CNil, String]) =>
        m.flatMap(f).flatMap(g) shouldBe m.flatMap(i => f(i).flatMap(g))
      }
    }
  }

  "toEither" should {
    "generate a Right from a success" in {
      forAll { (i:Int) =>
        EitherAcc.pure(i).toEither shouldBe Right(i)
      }
    }
    "generate a Left from an error" in {
      forAll { (s:String) =>
        EitherAcc.err(s).toEither shouldBe Left(Inl(s))
      }
    }
  }

  "flatMap in for" should {
    val successA: EitherAcc[String :+: CNil, Int] = EitherAcc.pure[String, Int](1)
    val successB: EitherAcc[Long :+: CNil, Int] = EitherAcc.pure[Long, Int](10)
    val successC: EitherAcc[Boolean :+: CNil, Int] = EitherAcc.pure[Boolean, Int](100)
    val failB: EitherAcc[Long :+: CNil, Int] = EitherAcc.err(-1L)
    val failB2: EitherAcc[Long :+: CNil, Int] = EitherAcc.err(-10L)

    "return correct value if all in success" in {
      val result: EitherAcc[String :+: Long :+: Boolean :+: CNil, Int] = for {
        a <- successA
        b <- successB
        c <- successC
      } yield a + b +c

      result shouldBe Success(111)
    }

    "return the first error " in {
      val result: EitherAcc[String :+: Long :+: Boolean :+: CNil, Int] = for {
        a <- successA
        b <- failB
        c <- successC
      } yield a + b +c

      result match {
        case Success(_) => fail("result should be an error")
        case Err(e) =>
          e.select[Long] shouldBe Some(-1L)
          e.select[String] shouldBe None
          e.select[Boolean] shouldBe None
      }
    }

    "aggregate same types of error" in {
      val result: EitherAcc[Long :+: String :+: CNil, Int] = for {
        a <- failB2
        b <- successA
        c <- failB
      } yield a + b +c

      result match {
        case Success(_) => fail("result should be an error")
        case Err(e) =>
          e.select[Long] shouldBe Some(-10L)
          e.select[String] shouldBe None
      }
    }
  }

}
