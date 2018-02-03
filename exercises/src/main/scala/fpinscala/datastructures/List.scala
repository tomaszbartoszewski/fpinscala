package fpinscala.datastructures

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


  // matchExample(List(1,2,3,4,5))
  def matchExample(is: List[Int]): Int = is match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def matchFewLists(): Unit = {
    def test(is: List[Int]): Unit = {
      println(is.toString())
      println(matchExample(is))
    }

    test(List(15, 2, 4, 5, 6, 7))
    test(Nil)
    test(List(1,2,3,4,5))
    test(List(3,2,1))
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

  // tail(List(1,2,3,4))
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

  // setHead(List(1,2,3,4), 9)
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(h, xs)
    }

  // drop(List(1,2,3,4,5,6) , 3)
  def drop[A](l: List[A], n: Int): List[A] =
  {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  // dropWhile(List(1,3,5,7,9,11), (x:Int) => x < 6)
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (f(x)) dropWhile(xs, f)
        else Cons(x, xs)
      }
    }
  }

  // dropWhile(List(1,3,5,7,9,11))(x => x < 6)
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Cons(h, t) if f(h) => dropWhile2(t)(f)
      case _ => as
    }
  }

  // init(List(1,2,3,4))
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  // length(List(1,2,3,4,5))
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x, y) => y + 1)

  // foldLeft(List(1,2,3,4,5), 0)((x,y) => x + y)
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go[A](l1: List[A], f1: (B, A) => B, r: B): B = l1 match {
      case Nil => r
      case Cons(x, xs) => go(xs, f1, f1(r, x))
    }

    go(l, f, z)
  }

  def sum_fl(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product_fl(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length_fl[A](l: List[A]): Int =
    foldLeft(l, 0)((y, x) => y + 1)

  def reverse[A](l: List[A]): List[A] =
    foldRight(l, Nil: List[A])((x,y) => append(y, Cons(x, Nil)))

  // datastructures.List.foldLeft_fr(datastructures.List(1,2,3,4,5), 0)((x,y) => x + y)
  def foldLeft_fr[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldRight[A, B](as :List[A], z: B)((x: A, y: B) => f(x, y))

  // datastructures.List.foldRight_fl(datastructures.List(1,2,3,4,5), 0)((x,y) => x + y)
  def foldRight_fl[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((x, y) => f(y, x))

  def reverse_fr_fl[A](l: List[A]): List[A] =
    foldRight_fl(l, Nil: List[A])((x,y) => append(y, Cons(x, Nil)))

  def reverse_fl_fr[A](l: List[A]): List[A] =
    foldLeft_fr(l, Nil: List[A])((x,y) => Cons(x, y))

  def append_fr[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, y) => Cons(x, y))

  def concatenate[A](list: List[List[A]]): List[A] =
    foldLeft(list, Nil: List[A])((res, head) => append(res, head))

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
