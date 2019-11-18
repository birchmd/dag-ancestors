package birchmd.dag

import cats.implicits._
import scala.collection.immutable.{HashSet, Queue}

class NaiveDag[F[_], A, Hash](
    node: Node[A, Hash],
    hashesMapping: Storage[F, Hash, A],
    childrenMapping: Storage[F, Hash, List[Hash]]
)(implicit monadError: cats.MonadError[F, Throwable])
    extends Dag[F, A, Hash] {

  private def childrenHashes(a: A): F[List[Hash]] =
    childrenMapping.lookup(node.id(a)) map {
      case None       => List.empty[Hash]
      case Some(list) => list
    }

  def children(a: A): F[List[A]] =
    childrenHashes(a).flatMap(_.traverse(hashesMapping.lookupUnsafe))

  def relation(x: A, y: A): F[Option[Dag.Relation]] =
    (node.rank(x), node.rank(y)) match {
      case (rx, ry) if rx == ry =>
        if (node.id(x) == node.id(y)) Dag.Relation.equal.some.pure[F]
        else none[Dag.Relation].pure[F]

      case (rx, ry) if rx < ry =>
        bfFind(y, idEquality(node.id(x)), rankBelow(node.rank(x)))
          .map(_.map(_ => Dag.Relation.ancestor))

      case (rx, ry) if rx > ry =>
        bfFind(x, idEquality(node.id(y)), rankBelow(node.rank(y)))
          .map(_.map(_ => Dag.Relation.descendant))
    }

  private def idEquality(target: Hash): A => Boolean = node.id(_) == target

  def rankBelow(target: Long): A => Boolean = node.rank(_) < target

  /** Breadth-first traversal through the DAG starting at `start`, following
    * parent links until we find an `A` with `property` evaluating to `true`, if
    * any. The `stop` condition allows for early termination by not traversing past
    * any `A`s where `stop` evaluates to `true`.
    */
  private def bfFind(
      start: A,
      property: A => Boolean,
      stop: A => Boolean
  ): F[Option[A]] =
    monadError.tailRecM((Queue(start), HashSet.empty[A]))(
      bfLoop(property, stop)(_)
    )

  private type LoopState = (Queue[A], HashSet[A])
  private def bfLoop(property: A => Boolean, stop: A => Boolean)(
      state: LoopState
  ): F[Either[LoopState, Option[A]]] = state match {
    case (q, visited) =>
      q.dequeueOption.fold(Right(none[A]).leftCast[LoopState].pure[F]) {
        case (curr, rem) =>
          if (visited(curr) || stop(curr))
            Left((rem, visited)).rightCast[Option[A]].pure[F]
          else if (property(curr)) Right(curr.some).leftCast[LoopState].pure[F]
          else
            node
              .parents(curr)
              .traverse(hashesMapping.lookupUnsafe)
              .map { parents =>
                Left((rem.enqueueAll(parents.filter(!stop(_))), visited + curr))
                  .rightCast[Option[A]]
              }
      }
  }
}
