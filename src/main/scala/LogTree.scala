package io.channing

import cats.data.WriterT
import cats.implicits._
import cats.{ Applicative, MonadError, Monoid, Show }

object LogTree {

  sealed trait Log

  private final case class LogMessage(msg: Vector[String]) extends Log

  private final case class Parent(label: String, children: Vector[Log]) extends Log

  object Log {
    implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
      override val empty: Log = LogMessage(Vector.empty)

      override def combine(x: Log, y: Log): Log =
        (x, y) match {
          case (a, b) if b == empty             => a
          case (a, b) if a == empty             => b
          case (LogMessage(a), LogMessage(b))   => LogMessage(a ++ b)
          case (Parent(a, childrenA), log: Log) => Parent(a, childrenA :+ log)
          case _                                => Parent("", Vector(x, y))
        }
    }

    implicit val show: Show[Log] = new Show[Log] {
      override def show(t: Log): String = doShow(t, 0)

      private def doShow(t: Log, depth: Int): String =
        t match {
          case LogMessage(msg) => msg.map(" " * depth + _).mkString("\n")
          case Parent(label, children) =>
            " " * depth + label + children
              .map(c => "\n" + doShow(c, depth + 1))
              .mkString
        }
    }
  }

  def log[F[_]: Applicative, A](f: F[A], msg: String): WriterT[F, Log, A] = WriterT.liftF[F, Log, A](f).tell(LogMessage(Vector(msg)))

  def log[F[_]: Applicative](msg: String): WriterT[F, Log, Unit] = WriterT.tell[F, Log](LogMessage(Vector(msg)))

  def parent[F[_]: Applicative](label: String): WriterT[F, Log, Unit] = WriterT.tell[F, Log](Parent(label, Vector.empty))

  implicit class ParentOps(s: String) {
    def ~<[F[_], A](a: WriterT[F, Log, A])(implicit me: MonadError[F, Log]): WriterT[F, Log, A] = parent[F](s) >> a
  }

  implicit class LogOps[F[_], A](f: F[A])(implicit me: MonadError[F, Log]) {
    def ~>(msg: String): WriterT[F, Log, A]       = log(f, msg)
    def ~~>(msg: A => String): WriterT[F, Log, A] = WriterT.liftF[F, Log, A](f).flatMap(a => log(f, msg(a)))
  }
}
