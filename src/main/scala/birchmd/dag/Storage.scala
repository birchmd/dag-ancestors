package birchmd.dag

import cats.implicits._

trait Storage[F[_], Key, Value] {
  def lookup(k: Key): F[Option[Value]]

  def lookupUnsafe(
      k: Key
  )(implicit monadError: cats.MonadError[F, Throwable]): F[Value] =
    lookup(k) flatMap {
      case None =>
        monadError.raiseError(
          new IllegalArgumentException(s"$k not found in storage")
        )

      case Some(value) => value.pure[F]
    }
}
