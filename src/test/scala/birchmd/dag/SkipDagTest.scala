package birchmd.dag

import cats.data.NonEmptyList
import cats.implicits._
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

  it should "return the powers in descending order" in forAll(
    Gen.choose(1L, 5000L)
  ) { n =>
    val decomposition = SkipDag.decompose(n)
    decomposition.zip(decomposition.tail).foreach {
      case (x, y) => x should be >= y
    }
  }

  "SkipDag.children" should "work correctly for a chain" in {
    implicit val mErr = Instances.monadErrorId
    val length = 10
    val underlyingDag = DagShapes.chain(length + 1)
    val dag = underlyingDag.skipDag[cats.Id]

    (0L until length.toLong).foreach { hash =>
      val node = underlyingDag.nodeByHash(hash)
      val nextNode = underlyingDag.nodeByHash(hash + 1)
      dag.children(node) shouldBe List(nextNode)
    }
  }

  it should "work correctly for a fork" in {
    implicit val mErr = Instances.monadErrorId
    val length = 3
    val underlyingDag = DagShapes.fork(length)

    val genesis = underlyingDag.genesis()
    val a = underlyingDag.nodeByHash(1)
    val b = underlyingDag.nodeByHash(2)
    val c = underlyingDag.nodeByHash(3)
    val d = underlyingDag.nodeByHash(4)
    val e = underlyingDag.nodeWithParents(NonEmptyList.of(c.id, d.id))

    val dag = underlyingDag.skipDag[cats.Id]
    dag.children(genesis).toSet shouldBe Set(a, b)
    dag.children(a) shouldBe List(c)
    dag.children(b) shouldBe List(d)
    dag.children(c) shouldBe List(e)
    dag.children(d) shouldBe List(e)
  }

  "SkipDag.relation" should "work correctly for a chain" in {
    implicit val mErr = Instances.monadErrorId
    val length = 10
    val underlyingDag = DagShapes.chain(length + 1)
    val dag = underlyingDag.skipDag[cats.Id]

    for {
      i <- (0L until length.toLong)
      node = underlyingDag.nodeByHash(i)
      _ = dag.relation(node, node) shouldBe Some(Dag.Relation.Equal)
      j <- (i + 1).until(length.toLong)
      futureNode = underlyingDag.nodeByHash(j)
      _ = dag.relation(node, futureNode) shouldBe Some(Dag.Relation.Ancestor)
      _ = dag.relation(futureNode, node) shouldBe Some(Dag.Relation.Descendant)
    } yield ()
  }

  it should "work correctly for a fork" in {
    implicit val mErr = Instances.monadErrorId
    val length = 3
    val underlyingDag = DagShapes.fork(length)

    val genesis = underlyingDag.genesis()
    val a = underlyingDag.nodeByHash(1)
    val b = underlyingDag.nodeByHash(2)
    val c = underlyingDag.nodeByHash(3)
    val d = underlyingDag.nodeByHash(4)
    val e = underlyingDag.nodeWithParents(NonEmptyList.of(c.id, d.id))

    val dag = underlyingDag.skipDag[cats.Id]
    dag.relation(genesis, genesis) shouldBe Some(Dag.Relation.Equal)
    dag.relation(a, b) shouldBe None
    dag.relation(b, a) shouldBe None
    dag.relation(a, d) shouldBe None
    dag.relation(d, a) shouldBe None
    List(a, b, c, d, e).foreach { x =>
      dag.relation(x, x) shouldBe Some(Dag.Relation.Equal)
      dag.relation(genesis, x) shouldBe Some(Dag.Relation.Ancestor)
      dag.relation(x, genesis) shouldBe Some(Dag.Relation.Descendant)
    }
    List(a, b, c, d).foreach { x =>
      dag.relation(x, e) shouldBe Some(Dag.Relation.Ancestor)
      dag.relation(e, x) shouldBe Some(Dag.Relation.Descendant)
    }
  }

  it should "be fast" in {
    val rand = new scala.util.Random()
    val dagWidth = 3
    val dagHeight = 100
    val dagGen = DagShapes.random(dagWidth, dagHeight)
    val nDag = dagGen.naiveDag[cats.Id](Instances.monadErrorId)
    val sDag = dagGen.skipDag[cats.Id](Instances.monadErrorId)
    val maxHash = dagGen.maxUsedIndex

    def compare(): (Long, Long) = {
      val a = dagGen.nodeByHash(rand.nextLong(maxHash))
      val b = dagGen.nodeByHash(rand.nextLong(maxHash))

      val nStart = System.nanoTime()
      val _ = nDag.relation(a, b)
      val nEnd = System.nanoTime()

      val sStart = System.nanoTime()
      val _ = sDag.relation(a, b)
      val sEnd = System.nanoTime()

      (nEnd - nStart, sEnd - sStart)
    }

    val warmup = 10
    val actual = 20

    (0 until warmup).foreach { _ =>
      val _ = compare()
    }
    val (nTotal, sTotal) = (0 until actual).foldLeft(0L -> 0L) {
      case ((nAcc, sAcc), _) =>
        val (nTime, sTime) = compare()
        (nAcc + nTime, sAcc + sTime)
    }

    val nAverage = nTotal.toDouble / (actual * 1000000L) // in units of ms
    val sAverage = sTotal.toDouble / (actual * 1000000L) // in units of ms
    println(s"NAIVE TIME: $nAverage ms")
    println(s"SKIP TIME: $sAverage ms")
  }
}
