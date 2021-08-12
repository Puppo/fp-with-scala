package example

enum Option[+A]:
    case Some(get: A)
    case None

    def map[B](f: A => B): Option[B] = this match
        case None => None
        case Some(a) => Some(f(a))

    def getOrElse[B>:A](default: => B): B = this match
        case None => default
        case Some(a) => a

    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

    def orElse[B>:A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = this match
        case Some(a) if f(a) => this
        case _ => None


object OptionProgram {


    def mainOption() = {

        val option: Option[Int] = Option.Some(1)
        val none: Option[Int] = Option.None

        println("Option some map %s".format(option.map(_ + 1)))
        println("Option none %s".format(none.map(_ + 1)))

        println("Option getOrElse map %s".format(option.getOrElse(0)))
        println("Option getOrElse %s".format(none.getOrElse(0)))
        

    }

}