
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
