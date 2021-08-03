package example

object MyModule {

    def abs(x: Int): Int = if (x < 0) -x else x

    def factorial(n: Int) = {
        def go(n: Int, acc: Int): Int =
            if (n == 0) acc
            else go(n - 1, n * acc)

        go(n, 1)
    }

    def fibonacci(n: Int) = {
        def go(n: Int): Int = n match {
            case 0 | 1 => n
            case _ => go(n - 1) + go(n - 2)
        }

        go(n)
    }

}

object FormatAbsAndFactorial {
    import MyModule._

    def formatAbs(x: Int) = {
        val msg = "The absolute value of %d is %d"
        msg.format(x, abs(x))
    }

    def formatFactorial(x: Int) = {
        val msg = "The factorial of %d is %d"
        msg.format(x, factorial(x))
    }

    def format(name: String, n: Int, f: Int => Int) = {
        val msg = "The %s of %d is %d"
        msg.format(name, n, f(n))
    }
}

object PolymorphicFunctions {

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = as.sortWith(ordered).sameElements(as)

    def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

    def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

    def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

}

object Program {

    import MyModule._
    import FormatAbsAndFactorial._
    import PolymorphicFunctions._

    def main(args: Array[String]): Unit = {
        println(formatAbs(-42))
        println(formatFactorial(7))

        println(format("absolute value", -42, abs))
        println(format("factorial", 7, factorial))
        println(format("fibonacci value", 6, fibonacci))

        val a = Array(1, 2, 3, 4, 5)
        val b = Array(1, 3, 2, 4, 5)

        def sortAsc = (x: Int, y: Int) => x < y
        println("Array a is sorted %s".format(isSorted(a, sortAsc)))
        println("Array b is sorted %s".format(isSorted(b, sortAsc)))
    }
}