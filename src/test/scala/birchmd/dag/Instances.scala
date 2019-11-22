package birchmd.dag

import cats.implicits._
import cats.{Monad, MonadError}
import scala.collection.immutable.Map
import scala.util.{Failure, Success, Try}

object Instances {
  case class InMem[F[_]: cats.Applicative, Key, Value](
      store: Map[Key, Value]
  ) extends Storage[F, Key, Value] {
    def lookup(k: Key): F[Option[Value]] = store.get(k).pure[F]
  }

  case class SimpleNode(id: Long, parents: List[Long], rank: Long)

  object NodeImpl extends Node[SimpleNode, Long] {
    def id(n: SimpleNode): Long = n.id
    def parents(n: SimpleNode): List[Long] = n.parents
    def rank(n: SimpleNode): Long = n.rank
  }

  def unsafeMErr[F[_]: Monad]: MonadError[F, Throwable] =
    new MonadError[F, Throwable] {
      def pure[A](x: A): F[A] = Monad[F].pure(x)
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
        Monad[F].tailRecM(a)(f)
      def raiseError[A](e: Throwable): F[A] = throw e
      def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] =
        Try(fa) match {
          case Success(x) => x
          case Failure(e) => f(e)
        }
    }

  val monadErrorId = unsafeMErr[cats.Id]
}
