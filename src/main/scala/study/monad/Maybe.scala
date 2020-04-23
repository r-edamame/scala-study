
package study.monad

sealed trait Maybe[+A] {
    def map[B](f: A => B): Maybe[B]
    def flatMap[B](f: A => Maybe[B]): Maybe[B]
}

object Maybe {
    case class Just[A](a: A) extends Maybe[A] {
        def map[B](f: A => B): Maybe[B] = Just(f(a))
        def flatMap[B](f: A => Maybe[B]): Maybe[B] = f(a)
    }
    case object Nothing extends Maybe[Nothing] {
        def map[B](f: Nothing => B): Maybe[B] = Nothing
        def flatMap[B](f: Nothing => Maybe[B]): Maybe[B] = Nothing
    }
}

object MaybeApp extends App {
    def lookup[A,B](key: A, list: List[(A, B)]): Maybe[B] = {
        list match {
            case Nil => Maybe.Nothing
            case (k,v) :: xs => {
                if (k == key) { Maybe.Just(v) } else { lookup(key, xs) }
            }
        }
    }
    def safeDiv(x: Int, y: Int): Maybe[Int] = {
        if (y==0) { Maybe.Nothing } else { Maybe.Just(x/y) }
    }

    val list = List(("x", 4), ("y", 3), ("z", 2))

    val r = lookup("x", list) flatMap { n =>
    lookup("y", list) flatMap { m =>
        safeDiv(n, m)
    }}
    println(s"result: $r")
}
