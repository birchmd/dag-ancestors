package birchmd.dag

import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class SkipDagTest
    extends FlatSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks {

  "SkipDag.decompose" should "write a number as a sum of powers of 2" in forAll(
    Gen.choose(1L, 5000L)
  ) { n =>
    SkipDag.decompose(n).sum shouldBe n
  }

  it should "not have more than 1 copy of any power except the largest one" in forAll(
    Gen.choose(1L, 5000L)
  ) { n =>
    val decomposition = SkipDag.decompose(n)
    SkipDag.powers.drop(1).foreach { p =>
      decomposition.count(_ == p) should be <= 1
    }
  }

}
