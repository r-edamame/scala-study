
package study.monad

import scala.language.higherKinds

trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
}

object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
}

trait Applicative[F[_]] extends Functor[F] {
    def ap[A,B](fa: F[A])(f: F[A => B]): F[B]
    def pure[A](a: A): F[A]

    def map[A,B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
}

object Applicative {
    def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]
}

trait Monad[F[_]] extends Applicative[F] {
    def bind[A,B](fa: F[A])(f: A => F[B]): F[B]
    def join[A](ffa: F[F[A]]): F[A] = bind(ffa)(x => x)

    def ap[A,B](fa: F[A])(f: F[A => B]): F[B] = bind(f)(map(fa)(_))
}

object Monad {
    def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]

    implicit def monadForOption = new Monad[Option] {
        def pure[A](a: A): Option[A] = Some(a)
        /*
        override def map[A,B](fa: Option[A])(f: A => B): Option[B] = {
            fa match {
                case Some(x) => Some(f(x))
                case None => None
            }
        }
        override def ap[A,B](fa: Option[A])(f: Option[A => B]): Option[B] = {
            f match {
                case Some(_f) => map(fa)(_f)
                case None => None
            }
        }
        */
        def bind[A,B](fa: Option[A])(f: A => Option[B]): Option[B] = {
            fa match {
                case Some(x) => f(x)
                case None => None
            }
        }
    }
}


object MonadApp extends App {
    val operators = Map(
        "+" -> ((x: Int, y: Int) => Some(x + y)),
        "-" -> ((x: Int, y: Int) => Some(x - y)),
        "*" -> ((x: Int, y: Int) => Some(x * y)),
        "/" -> ((x: Int, y: Int) => if (y==0) { None } else { Some(x / y) })
    )
    val variables = Map(
        "x" -> 3,
        "y" -> 8,
        "z" -> 0
    )

    def calc(op: String, v1: String, v2: String)(implicit M: Monad[Option]): Option[Int] = {
        M.bind(operators get op)({ _op =>
        M.bind(variables get v1)({ _v1 =>
        M.bind(variables get v2)({ _v2 => {
            _op(_v1, _v2)
        }})})})
    }

    val r1 = calc("+", "x", "y")
    println(s"r1 = $r1")

    val r2 = calc("/", "x", "z")
    println(s"r2 = $r2")

    val r3 = calc("?", "x", "y")
    println(s"r3 = $r3")
}
