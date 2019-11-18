package birchmd.dag

import cats.implicits._

trait Dag[F[_], A, Hash] {
  def children(a: A): F[List[A]]
  def relation(x: A, y: A): F[Option[Dag.Relation]]

  def isAncestor(x: A, y: A)(implicit functor: cats.Functor[F]): F[Boolean] =
    relation(x, y) map {
      case None                          => false
      case Some(Dag.Relation.Equal)      => true
      case Some(Dag.Relation.Ancestor)   => true
      case Some(Dag.Relation.Descendant) => false
    }
}

object Dag {
  sealed trait Relation
  object Relation {
    def equal: Dag.Relation = Equal
    def ancestor: Dag.Relation = Ancestor
    def descendant: Dag.Relation = Descendant

    /** x == y */
    case object Equal extends Dag.Relation

    /** There exists a (non-empty) path from y to x via parent links */
    case object Ancestor extends Dag.Relation

    /** There exists a (non-empty) path from x to y via parent links */
    case object Descendant extends Dag.Relation
  }
}
