package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // result will be the RHS of the third case - 1+2=3
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, as) => as
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, as) => Cons(h, as)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case 1 => tail(l)
    case _ => drop(tail(l), n-1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, as) => if (f(a)) dropWhile(as, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(a, as) => as match { case Nil => Nil; case _ => Cons(a, init(as)) }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, total: Int) => total + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(l: List[A], acc: B): B = l match {
      case Nil => acc
      case Cons(a, as) => loop(as, f(acc, a))
    }

    loop(l, z)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  def main(args: Array[String]): Unit = {
    require(x == 3)
    println("Verified x == 3 in pattern matching exercise (3.1)")

    println("Testing tail:")
    println("Expected: Nil, Cons(2,Cons(3,Nil))")
    println("Actual:   %s, %s".format(tail(Nil), tail(List(1, 2, 3))))

    println("Testing drop:")
    println("Expected: Cons(4,Cons(5,Nil))")
    println("Actual:   %s".format(drop(List(1, 2, 3, 4, 5), 3)))

    println("Testing dropWhile:")
    println("Expected: Cons(3,Cons(4,Cons(5,Nil)))")
    println("Actual:   %s".format(dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i != 3)))

    println("Testing init:")
    println("Expected: Cons(1,Cons(2,Cons(3,Nil)))")
    println("Actual:   %s".format(init(List(1, 2, 3, 4))))

    println(s"Testing foldRight behaviour: ${foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_))}")

    println("Testing length:")
    println("Expected: 0 1 5")
    println("Actual:   %d %d %d".format(length(Nil), length(List(1)), length(List(6, 7, 8, 4, 3))))

    println("Testing foldLeft with sum:")
    println("Expected: 10")
    println("Actual:   %d".format(foldLeft(List(1, 2, 3, 4), 0)(_ + _)))
  }
}
