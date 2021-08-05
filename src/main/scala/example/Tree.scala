package example

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object  Tree { 

    def size[A](as: Tree[A]): Int = as match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
    }

    def maximum(as: Tree[Int]): Int = as match {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
    }

    def depth(as: Tree[Int]): Int = as match {
        case Leaf(_) => 0
        case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    def map[A, B](as: Tree[A])(f: A => B): Tree[B] = as match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    def fold[A, B](as: Tree[A])(f: A => B)(g: (B,B) => B): B = as match {
        case Leaf(v) => f(v)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def foldSize[A](as: Tree[A]) = fold(as)(_ => 1)(1 + _ + _)

    def foldMaximum(as: Tree[Int]) = fold(as)(a => a)((l,r) => l max r)

    def foldDepth[A](as: Tree[A]) = fold(as)(_ => 0)((l,r) => 1 + (l max r))

    def foldMap[A, B](as: Tree[A])(f: A => B): Tree[B] = fold(as)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}

object TreeProgram {

    def mainTree() = {

        val tree: Tree[Int] = 
        Branch(
            Branch(
                Branch(
                    Leaf(1),
                    Branch(
                        Leaf(2),
                        Branch(
                            Leaf(3),
                            Branch(
                                Leaf(9),
                                Branch(
                                    Leaf(10),
                                    Leaf(11),
                                )
                            )
                        )
                    )
                ),
                Branch(
                    Branch(
                        Leaf(4),
                        Branch(
                            Leaf(5),
                            Leaf(6)
                        )
                    ),
                    Leaf(7)
                )
            ),
            Leaf(8)
        )

        println("Tree size %d".format(Tree.size(tree)))
        println("Tree maximum %d".format(Tree.maximum(tree)))
        println("Tree depth %d".format(Tree.depth(tree)))
        println("Tree map %s".format(Tree.map(tree)(_ * 2)))


        println("Tree foldSize %d".format(Tree.foldSize(tree)))
        println("Tree foldMaximum %d".format(Tree.foldMaximum(tree)))
        println("Tree foldDepth %d".format(Tree.foldDepth(tree)))
        println("Tree foldMap %s".format(Tree.foldMap(tree)(_ * 2)))

    }

}