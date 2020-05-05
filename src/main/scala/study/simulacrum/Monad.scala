
package study.simulacrum

import scala.language.higherKinds
import scala.language.implicitConversions

import simulacrum._


@typeclass trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
}

object Functor {

    implicit def maybeFunctor: Functor[Maybe] = new Functor[Maybe] {
        def map[A,B](fa: Maybe[A])(f: A => B): Maybe[B] = {
            fa match {
                case Maybe.Just(v) => Maybe.Just(f(v))
                case Maybe.Nothing => Maybe.Nothing
            }
        }
    }
}



sealed trait Maybe[+A]

object Maybe {
    case class Just[A](value: A) extends Maybe[A]
    case object Nothing extends Maybe[scala.Nothing]
}



object FunctorApp extends App {
    import Functor.ops._

    val m: Maybe[Int] = Maybe.Just(3)

    println(m.map((x) => x * x))
}
