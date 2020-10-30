package io.channing

import cats.implicits._
import cats.Monad
import cats.data.WriterT
import LogTree._

object Spike extends App {

  def foo[F[_]: Monad]: WriterT[F, Log, Int] =
    "Start Process" ~< {
      for {
        a <- doA[F]
        b <- doB[F]
      } yield a + b
    }

  def doA[F[_]: Monad]: WriterT[F, Log, Int] =
    for {
      c <- 3.pure[F] ~> "Calc c"
      d <- 4.pure[F] ~> "Calc d"
    } yield c + d

  def doB[F[_]: Monad]: WriterT[F, Log, Int] =
    "Doing B" ~< {
      for {
        e <- 5.pure[F] ~> "Calc e"
        f <- 6.pure[F] ~> "Calc f"
      } yield e + f
    }

  val (l, v) = foo[cats.Id].run
  println(v)
  println(l.show)
}
