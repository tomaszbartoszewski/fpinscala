package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toList2: List[A] = foldRight(Nil: List[A])(_ :: _)

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).take(5).toList2
  def take(n: Int): Stream[A] = {
    def go(i: Int, s: Stream[A]): Stream[A] = i match {
      case 0 => Empty
      case _ => s match {
        case Empty => Empty
        case Cons(h, t) => Cons(h, () => go(i-1, t()))
      }
    }

    go(n, this)
  }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).drop(5).toList
  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t() drop(n-1)
    case _ => this
  }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).takeWhile(_ < 5).toList
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).forAll(_ > 0)
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if (!p(h())) => { println("false"); false }
    case Cons(_, t) => { println("Check tail"); t() forAll(p) }
    case _ => { println("true"); true }
  }

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).takeWhile_foldRight(_ < 5).toList
  def takeWhile_foldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Cons(() => a, () => b) else b)

  // fpinscala.laziness.Stream(7,8,9).headOption
  // fpinscala.laziness.Stream().headOption
  def headOption: Option[A] = 
    foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).map(_ + 1)
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).filter(_ % 2 == 0)
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).append(Stream(10,11,12))
  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)(cons(_, _))

  // fpinscala.laziness.Stream(1,2,3,4,5,6,7,8,9).flatmap(i => fpinscala.laziness.Stream(i,i,i))
  def flatmap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  // fpinscala.laziness.Stream.constant(5).take(6).toList
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  val ones: Stream[Int] = Stream.cons(1, ones)

  // fpinscala.laziness.Stream.from(6).take(5).toList
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}