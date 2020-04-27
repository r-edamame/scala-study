
package study.typeclass

trait Monoid[T] {
    def zero: T
    def add(x: T, y: T): T
}

object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]

    implicit object monoidForString extends Monoid[String] {
        def zero: String = ""
        def add(x: String, y: String) = x + y
    }

    def sum[A: Monoid](list: List[A]): A = {
        list.reduceLeft((a,b) => Monoid[A].add(a,b))
    }

    implicit def monoidForOption[A: Monoid] = new Monoid[Option[A]] {
        def zero: Option[A] = None
        def add(x: Option[A], y: Option[A]) = {
            x match {
                case None => None
                case Some(_x) => {
                    y match {
                        case None => None
                        case Some(_y) => {
                            val m = implicitly[Monoid[A]]
                            Some(m.add(_x, _y))
                        }
                    }
                }
            }
        }
    }
}


object MonoidApp extends App {
    val M = Monoid[String]
    val s1 = "Hello"
    val s2 = "World"

    println(M.add(s1, s2))
}

object MonoidApp2 extends App {
    val M = Monoid[Option[String]]
    val m1 = Some("Hello")
    val m2 = Some("World")

    println(M.add(m1, m2))
}


object MonoidApp3 extends App {
    final case class Pair[A,B](fst: A, snd: B)

    object Pair {
        implicit def monoidForPair[A: Monoid,B: Monoid] = new Monoid[Pair[A,B]] {
            def zero: Pair[A,B] = Pair(Monoid[A].zero, Monoid[B].zero)
            def add(x: Pair[A,B], y: Pair[A,B]): Pair[A,B] = {
                Pair(Monoid[A].add(x.fst, y.fst), Monoid[B].add(x.snd, y.snd))
            }
        }
    }

    val m: Pair[String, String] = Pair("Hello", "World")
    val n: Pair[String, String] = Pair(" ", "!")

    val Pair(h, w) = Monoid[Pair[String,String]].add(m, n)
    println(Monoid[String].add(h,w))
}

object MonoidApp4 extends App {
    def list = List("Hello", " ", "World", "!")
    println(Monoid.sum(list))
}
