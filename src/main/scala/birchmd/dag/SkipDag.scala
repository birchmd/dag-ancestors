package birchmd.dag

import cats.implicits._

class SkipDag[F[_], A, Hash](
    node: Node[A, Hash],
    hashesMapping: Storage[F, Hash, A],
    childrenMapping: Storage[F, Hash, List[Hash]],
    ancestorMapping: Storage[F, (Hash, Long), List[Hash]] // cache all ancestors at rank 2^n back
)(implicit monadError: cats.MonadError[F, Throwable])
    extends NaiveDag[F, A, Hash](node, hashesMapping, childrenMapping) {

  override def relation(x: A, y: A): F[Option[Dag.Relation]] =
    (node.rank(x), node.rank(y)) match {
      case (rx, ry) if rx == ry =>
        if (node.id(x) == node.id(y)) Dag.Relation.equal.some.pure[F]
        else none[Dag.Relation].pure[F]

      case (rx, ry) if rx < ry =>
        val rankDiff = ry - rx
        val plan = SkipDag.decompose(rankDiff)
        val target = node.id(x)

        monadError
          .tailRecM(List(y) -> plan)(relationLoop(target)(_))
          .map(_.map(_ => Dag.Relation.ancestor))

      case (rx, ry) if rx > ry =>
        val rankDiff = rx - ry
        val plan = SkipDag.decompose(rankDiff)
        val target = node.id(y)

        monadError
          .tailRecM(List(x) -> plan)(relationLoop(target)(_))
          .map(_.map(_ => Dag.Relation.descendant))
    }

  private type LoopState = (List[A], List[Long])
  private def relationLoop(
      target: Hash
  )(state: LoopState): F[Either[LoopState, Option[A]]] = state match {
    case (candidates, Nil) =>
      Right(candidates.find(node.id(_) == target)).leftCast[LoopState].pure[F]

    case (candidates, step :: rem) =>
      candidates
        .flatTraverse(c => ancestorMapping.lookupUnsafe(node.id(c) -> step))
        .flatMap(_.distinct.traverse(hashesMapping.lookupUnsafe))
        .map { newCandidates =>
          Left(newCandidates -> rem).rightCast[Option[A]]
        }
  }

}

object SkipDag {
  val powers = (1 until 10).scanLeft(1L) { case (x, _) => x * 2L }.reverse

  /** Write `n` as a sum of powers of 2 */
  def decompose(n: Long): List[Long] =
    powers
      .foldLeft(n -> List.empty[Long]) {
        case ((m, acc), power) =>
          val numPower = m / power
          val remaining = m - power * numPower
          (remaining, List.fill(numPower.toInt)(power) ::: acc)
      }
      ._2
      .reverse
}
