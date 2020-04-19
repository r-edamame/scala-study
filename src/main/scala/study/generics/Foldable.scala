
package study.generics

import scala.language.higherKinds

trait Foldable[F[_]] {
    def foldl[A,B](fa: F[A])(v: B, f: (B, A) => B): B
    def foldr[A,B](fa: F[A])(v: B, f: (A, B) => B): B
}

object Foldable {
    implicit val foldableForList = new Foldable[List] {
        def foldl[A,B](fa: List[A])(v: B, f: (B, A) => B): B = {
            fa match {
                case Nil => v
                case x :: xs => foldl(xs)(f(v, x), f)
            }
        }

        def foldr[A,B](fa: List[A])(v: B, f: (A, B) => B): B = {
            fa match {
                case Nil => v
                case x :: xs => f(x, foldr(xs)(v, f))
            }
        }
    }
}

object FoldableApp extends App {
    def reverse[A](l: List[A])(implicit f: Foldable[List]): List[A] = {
        f.foldl(l)(Nil, (acc: List[A], cur: A) => cur :: acc)
    }

    val l = List(1, 2, 3)
    println(l)

    val reversed = reverse(l)
    println(s"reversed $reversed")
}
