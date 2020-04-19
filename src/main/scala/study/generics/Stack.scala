
package study.generics

trait Stack[A] {
    def push(a: A): Unit
    def pop(): Option[A]
}

class ListStack[A] extends Stack[A] {
    var values: List[A] = Nil

    def push(a: A): Unit = {
        values = a :: values
    }

    def pop(): Option[A] = {
        values match {
            case Nil => None
            case x :: xs => {
                values = xs
                Some(x)
            }
        }
    }
}

object StackApp extends App {
    val stack: Stack[Int] = new ListStack[Int]

    println("push 1 to stack")
    stack.push(1)
    println("push 2 to stack")
    stack.push(2)
    println("push 3 to stack")
    stack.push(3)

    val v1 = stack.pop
    println(s"poped $v1")
    val v2 = stack.pop
    println(s"poped $v2")
    val v3 = stack.pop
    println(s"poped $v3")
    val v4 = stack.pop
    println(s"poped $v4")
}
