package example


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }

    def setHead[A](as: List[A], h: A): List[A] = as match {
        case Nil => sys.error("setHead on empty list")
        case Cons(_,t) => Cons(h, as)
    }

    def drop[A](as: List[A], n: Int): List[A] =
        if (n <= 0) as
        else as match {
            case Nil => Nil
            case Cons(_,t) => drop(t, n-1)
        }

    def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
        case Cons(h, t) if f(h) => dropWhile(t)(f)
        case _ => as
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
    }

    def init[A](l: List[A]): List[A] = l match {
        case Nil => sys.error("init of empty list")
        case Cons(h,Nil) => Nil
        case Cons(h,t) => Cons(h, init(t))
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case Cons(h,t) => f(h, foldRight(t, z)(f))
    }

    def sum2(as: List[Int]) = foldRight(as, 0)(_ + _)

    def product2(as: List[Double]) = foldRight(as, 1.0)(_ * _)

    def lengthRight[A](as: List[A]):Int = foldRight(as, 0)((_, acc) => acc + 1)

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
        case Nil => z
        case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }

    def sum3(as: List[Int]) = foldLeft(as, 0)(_ + _)

    def product3(as: List[Double]) = foldLeft(as, 1.0)(_ * _)

    def lengthLeft[A](as: List[A]):Int = foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]) = foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

    def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A,B) => B): B =
        foldLeft(as, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

    def appendWithFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_,_))

    def concat[A](as: List[List[A]]): List[A] = foldRight(as, Nil:List[A])(appendWithFoldRight)

    def add1(as: List[Int]): List[Int] = foldRight(as, List[Int]())((h, t) => Cons(h + 1, t))

    def doubleToString(as: List[Double]): List[String] = foldRight(as, List[String]())((h, t) => Cons(h.toString(), t))

    def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, List[B]())((h, t) => Cons(f(h), t))
    
    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List[A]())((h, t) => if (f(h)) Cons(h, t) else t)

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

    def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)((a) => if (f(a)) List(a) else List())

    def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }

    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a,b) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
        case (_,Nil) => true
        case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
        case _ => false
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
        case Nil => sub == Nil
        case _ if startsWith(sup, sub) => true
        case Cons(h,t) => hasSubsequence(t, sub)
    }

}

object ListProgram {

    import List._

    def mainList() = {

    val x = List(1,2,3,4,5) match {
            case Cons(x, Cons(2, Cons(4, _))) => x
            case Nil => 42
            case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
            case Cons(h, t) => h + sum(t)
            case _ => 101
        }
        println("x: %s".format(x))

        val ints: List[Int] = List(1,2,3,4,5)
        val doubles: List[Double] = List(1.0,2.0,3.0,4.0,5.0)
        println("sum: %s".format(sum(ints)))
        println("sum: %s".format(sum2(ints)))
        println("sum: %s".format(sum3(ints)))
        println("product: %s".format(product(doubles)))
        println("product: %s".format(product2(doubles)))
        println("product: %s".format(product3(doubles)))
        println("lengthRight ints: %s".format(lengthRight(ints)))
        println("lengthRight doubles: %s".format(lengthRight(doubles)))
        println("lengthLeft ints: %s".format(lengthLeft(ints)))
        println("lengthLeft doubles: %s".format(lengthLeft(doubles)))

        println("doubleToString doubles: %s".format(doubleToString(doubles)))
        println("map doubles: %s".format(map(doubles)(d => d * 2.0)))
        println("filter doubles: %s".format(filter(doubles)(d => d % 2 == 0)))
        println("filterWithFlatMap doubles: %s".format(filterWithFlatMap(doubles)(d => d % 2 == 0)))
        
        println("reverse ints: %s".format(reverse(ints)))
        println("reverse doubles: %s".format(reverse(doubles)))

        println("appendWitFoldRight ints: %s".format(append(ints, List(6, 7))))
        println("appendWitFoldRight doubles: %s".format(append(doubles, List(6.0, 7.0))))
        
        println("foldRight: %s".format(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))))

        val list: List[Int] = List(1,2,3,4,5)
        val list1: List[Int] = List(6,7,8,9,10)
        
        println("tail: %s".format(tail(list)))
        println("setHead: %s".format(setHead(list, 4)))
        println("drop: %s".format(drop(list, 1)))
        println("dropWhile: %s".format(dropWhile(list)(x => x < 3)))
        println("append: %s".format(append(list, list1)))
        println("init: %s".format(init(list)))

        println("list: %s".format(list))
        println("list1: %s".format(list1))

        println("add1: %s".format(add1(list)))

        println("concat: %s".format(concat(List(List(1, 2), List(3, 4), List(5, 6)))))
        println("flatMap: %s".format(flatMap(List(1, 2, 3))((i) => List(i, i))))
        println("addPairwise: %s".format(addPairwise(List(1, 2, 3), List(4, 5, 6))))
        println("zipWith: %s".format(zipWith(List(1, 2, 3), List(4, 5, 6))((a,b) => a + b)))
        println("hasSubsequence: %s".format(hasSubsequence(List(1, 2, 3, 4), List(1, 2))))
        println("hasSubsequence: %s".format(hasSubsequence(List(1, 2, 3, 4), List(3, 4))))
        println("hasSubsequence: %s".format(hasSubsequence(List(1, 2, 3, 4), List(4))))
        println("hasSubsequence: %s".format(hasSubsequence(List(1, 2, 3, 4), Nil:List[Int])))
        println("hasSubsequence: %s".format(hasSubsequence(List(1, 2, 3, 4), List(4, 3, 4))))

    }
}