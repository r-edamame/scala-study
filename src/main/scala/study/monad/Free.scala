
package study.monad

import scala.language.higherKinds

sealed trait Free[F[_], +A]

object Free {
    case class Pure[F[_], A](a: A) extends Free[F,A]
    case class More[F[_], A](f: F[Free[F,A]]) extends Free[F,A]

    implicit def monadForFree[F[_]: Functor]: Monad[({type T[A] = Free[F,A]})#T] =
        new Monad[({type T[A] = Free[F,A]})#T] {
            def pure[A](a: A): Free[F,A] = Pure(a)

            override def map[A,B](fa: Free[F,A])(f: A => B): Free[F,B] = {
                fa match {
                    case Pure(x) => Pure(f(x))
                    case More(m) => More(Functor[F].map(m)(map(_)(f)))
                }
            }

            def bind[A,B](fa: Free[F,A])(f: A => Free[F,B]): Free[F,B] = {
                fa match {
                    case Pure(x) => f(x)
                    case More(m) => More(Functor[F].map(m)(bind(_)(f)))
                }
            }
        }
}

sealed trait C[+A]
object C {
    case class PutLine[A](c: String, cnt: A) extends C[A]
    case class GetLine[A](cnt: String => A) extends C[A]

    implicit def functorForC: Functor[C] = new Functor[C] {
        def map[A,B](fa: C[A])(f: A => B): C[B] = {
            fa match {
                case PutLine(c, cnt) => PutLine(c, f(cnt))
                case GetLine(cnt) => GetLine(s => f(cnt(s)))
            }
        }
    }

    def putLine(s: String): Free[C,Unit] = Free.More(PutLine[Free[C,Unit]](s,Free.Pure(())))
    def getLine: Free[C,String] = Free.More(GetLine[Free[C,String]](s => Free.Pure(s)))

    def run[A](m: Free[C,A]): A = {
        m match {
            case Free.Pure(x) => x
            case Free.More(PutLine(l, cnt)) => {
                println(l)
                run(cnt)
            }
            case Free.More(GetLine(cnt)) => {
                val line = io.StdIn.readLine
                run(cnt(line))
            }
        }
    }
}


object FreeApp extends App {
    val prog: Free[C,Unit] = {
        val M = Monad[({ type T[x] = Free[C,x]})#T]
        M.bind[String,Unit](C.getLine)(l => (C.putLine(l + l)))
    }

    C.run(prog)
}
