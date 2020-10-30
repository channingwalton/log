package io.channing

import cats.implicits._
import cats.MonadError
import cats.data.WriterT
import LogTree._

object Spike extends App {

  def foo[F[_]](implicit me: MonadError[F, Log]): WriterT[F, Log, Int] =
    "Start Process" ~< {
      for {
        a <- doA[F]
        b <- doB[F]
      } yield a + b
    }

  def doA[F[_]](implicit me: MonadError[F, Log]): WriterT[F, Log, Int] =
    "Doing A" ~< {
      for {
        c <- 3.pure[F] ~~> { "Calc c: " + _ }
        d <- 4.pure[F] ~~> { "Calc d: " + _ }
      } yield c + d
    }

  def doB[F[_]](implicit me: MonadError[F, Log]): WriterT[F, Log, Int] =
    "Doing B" ~< {
      for {
        e <- 5.pure[F] ~~> { "Calc e: " + _ }
        f <- 6.pure[F] ~~> { "Calc f: " + _ }
      } yield e + f
    }

  type MyF[A] = Either[Log, A]
  val x: MyF[(Log, Int)] = foo[MyF].run

  println(x.map(_._1).merge.show)
  /*
  Start Process
 Doing A
  Calc c: 3
  Calc d: 4

  Doing B
   Calc e: 5
   Calc f: 6

   */
}
